# windows-camera-class-filter-driver
Camera class filter driver for Windows.

# Generate catalog (.cat) file from package:
```
inf2cat /driver:<package_path> /os:<arch>
```
[https://msdn.microsoft.com/en-us/library/windows/hardware/ff547089(v=vs.85).aspx](https://msdn.microsoft.com/en-us/library/windows/hardware/ff547089(v=vs.85).aspx)

# Enable test signing:
```
bcdedit /set testsigning on
```

# Driver setup:
Use the ccfltr-console tool to install/uninstall the driver. Copy the tool to the directory of the .sys and .inf file.

### Installation:
```
ccfltr-console.exe /install
```

### Uninstallation:
```
ccfltr-console.exe /uninstall
```
