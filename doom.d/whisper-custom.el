;;; ../../.dotconfigs/doom.d/whisper-custom.el -*- lexical-binding: t; -*-

(defun whisper-custom-run (&optional arg)
  "Transcribe/translate audio using whisper.

When ARG is given, uses a local file as input. Otherwise records the audio.

This is a dwim function that does different things depending on current state:

- When inference engine (whisper.cpp) isn't installed, installs it first.
- When speech recognition model isn't available, downloads it.
- When installation/download is already in progress, cancels those.
- When installation is valid, starts recording audio.
- When recording is in progress, stops it and starts transcribing.
- When transcribing is in progress, cancels it."
  (interactive "P")
  (if (process-live-p whisper--transcribing-process)
      (when (yes-or-no-p "A transcribing is already in progress, kill it?")
        (kill-process whisper--transcribing-process))

    (cond
     ((process-live-p whisper--recording-process)
      (interrupt-process whisper--recording-process))
     ((and (buffer-live-p whisper--compilation-buffer)
           (process-live-p (get-buffer-process whisper--compilation-buffer)))
      (when-let ((proc (get-buffer-process whisper--compilation-buffer)))
	(interrupt-process proc)))
     (t
      (setq whisper--point-buffer (current-buffer))
      (run-hooks 'whisper-before-transcription-hook)
      (when whisper-install-whispercpp
        (whisper--check-model-consistency))
      (setq-default whisper--ffmpeg-input-file nil)
      (if arg
          (progn
            (setq-default whisper--ffmpeg-input-file arg)
            (unless (file-readable-p arg)
              (error "Media file doesn't exist or isn't readable"))))
      (setq whisper--using-whispercpp nil)
      (if whisper-install-whispercpp
          (whisper--check-install-and-run nil "whisper-start")
        ;; if user is bringing their own inference engine, we at least check the command exists
        (let ((command (car (whisper-command whisper--temp-file))))
          (if (or (file-exists-p command)
                  (executable-find command))
              (whisper--record-audio)
            (error (format "Couldn't find %s in PATH, nor is it a file" command)))))))))
