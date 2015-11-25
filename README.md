# windows-camera-class-filter-driver
Camera class filter driver for Windows.

# Driver setup:
Use the ccfltr-console tool to install the driver. Copy the tool to the directory of the .sys and .inf file and run on an elevated command prompt.
### Installation:
```
ccfltr-console.exe /install
```
### Uninstallation:
```
ccfltr-console.exe /uninstall
```

# Generate catalog (.cat) file from package:
```
inf2cat /driver:<package_path> /os:<arch>
```
[https://msdn.microsoft.com/en-us/library/windows/hardware/ff547089(v=vs.85).aspx](https://msdn.microsoft.com/en-us/library/windows/hardware/ff547089(v=vs.85).aspx)
