#!/usr/bin/env bash
# __  __           _        _
#|  \/  | __ _  __| | ___  | |__  _   _
#| |\/| |/ _` |/ _` |/ _ \ | '_ \| | | |
#| |  | | (_| | (_| |  __/ | |_) | |_| |
#|_|  |_|\__,_|\__,_|\___| |_.__/ \__, |
#                                 |___/
#          _ _ _     _     _        _       _   __
#  ___ ___| (_) |__ (_)___| |_ _ __(_) __ _| |  \ \
# / __/ _ \ | | '_ \| / __| __| '__| |/ _` | | (_) |
#| (_|  __/ | | |_) | \__ \ |_| |  | | (_| | |  _| |
# \___\___|_|_|_.__/|_|___/\__|_|  |_|\__,_|_| (_) |
#                                               /_/

# Check if script is running as root
if [[ $EUID -ne 0 ]]; then
   echo "This script must be run as root or with sudo."
   exit 1
fi

# Ask user for the partition where the Btrfs device is located
read -p "Enter the partition where the Btrfs device is located (e.g., /dev/sda1): " partition

# Check if the partition exists
if [[ ! -b $partition ]]; then
   echo "Invalid partition specified. Please provide a valid partition."
   exit 1
fi

# Mount the Btrfs device location to /mnt
mount "$partition" /mnt

# Ask user for the location of the broken subvolume
read -p "Enter the location of the broken subvolume (e.g., /mnt/broken_subvolume): " broken_subvolume_location

# Check if the broken subvolume location exists
if [[ ! -d $broken_subvolume_location ]]; then
   echo "Invalid broken subvolume location specified. Please provide a valid directory."
   exit 1
fi

# Ask user for the location of snapshots
read -p "Enter the location of the snapshots (e.g., /mnt/snapshots): " snapshot_location

# Check if the snapshot location exists
if [[ ! -d $snapshot_location ]]; then
   echo "Invalid snapshot location specified. Please provide a valid directory."
   exit 1
fi

# List available snapshots
echo "Available snapshots:"
ls -1 "$snapshot_location"

# Ask user which snapshot to restore
read -p "Enter the name of the snapshot to restore: " snapshot_name

# Check if the snapshot exists
if [[ ! -d "$snapshot_location/$snapshot_name" ]]; then
   echo "Invalid snapshot specified. Please provide a valid snapshot name."
   exit 1
fi

# Move broken subvolume to backup location
mv "$broken_subvolume_location" "$broken_subvolume_location.broken"

# Create a read-write subvolume from the selected snapshot
btrfs subvolume snapshot "$snapshot_location/$snapshot_name" "$partition"

echo "Restoration complete!"
