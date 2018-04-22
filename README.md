[![Build status](https://ci.appveyor.com/api/projects/status/ldqswni016jufm6p/branch/master?svg=true)](https://ci.appveyor.com/project/flowerinthenight/windows-camera-class-filter-driver/branch/master)

## Overview

**Disclaimer:** This driver code was intended to be for POC only and was never released to production. If you're looking at the codes, you probably know that this type of driver is generally undocumented and some bits are achieved using reverse engineering.

This driver was tested (though not extensively; it failed on HLK) on Windows 7, 8 and 10 (2015 release) using the ThinkPad X and T series (2015 and older) on both proprietary camera drivers and Microsoft's default UVC driver. 

## Generate catalog (.cat) file from package
```
inf2cat /driver:<package_path> /os:<arch>
```
[https://msdn.microsoft.com/en-us/library/windows/hardware/ff547089(v=vs.85).aspx](https://msdn.microsoft.com/en-us/library/windows/hardware/ff547089(v=vs.85).aspx)

## Enable test signing
```
bcdedit /set testsigning on
```

## Driver setup
Use the [ccfltr-console](https://github.com/flowerinthenight/windows-camera-class-filter-driver/tree/master/ccfltr-console) tool to install/uninstall the driver. Copy the tool to the directory of the .sys and .inf file.

### Installation
```
ccfltr-console.exe /install
```

### Uninstallation
```
ccfltr-console.exe /uninstall
```

## License

[The MIT License](./LICENSE.md)
