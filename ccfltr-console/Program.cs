/*
 *		Copyright (C) 2015 Chew Esmero
 *
 *		This program is free software: you can redistribute it and/or modify
 *		it under the terms of the GNU General Public License as published by
 *		the Free Software Foundation, either version 3 of the License, or
 *		(at your option) any later version.
 *
 *		This program is distributed in the hope that it will be useful,
 *		but WITHOUT ANY WARRANTY; without even the implied warranty of
 *		MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *		GNU General Public License for more details.
 *
 *		You should have received a copy of the GNU General Public License
 *		along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Security.Principal;
using System.Text;
using System.Threading.Tasks;

namespace ccfltr_console
{
    class Program
    {
        private static void Help()
        {
            Console.WriteLine("Synopsis:\n");
            Console.WriteLine("    ccfltr-console.exe /<option>\n");
            Console.WriteLine("Options:\n");
            Console.WriteLine("    /install - installs ccfltr filter package.");
            Console.WriteLine("    /uninstall - uninstalls ccfltr filter package.");
        }

        private static bool IsAdmin()
        {
            WindowsIdentity identity = WindowsIdentity.GetCurrent();
            WindowsPrincipal principal = new WindowsPrincipal(identity);
            bool isElevated = principal.IsInRole(WindowsBuiltInRole.Administrator);

            if (isElevated) return true;

            return false;
        }

        private static bool SetupUpperFilters()
        {
            bool result = false;
            string cameraClass = @"HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Class\{6bdd1fc6-810f-11d0-bec7-08002be2092f}";

            int ccfltr = -1;
            int ksthunk = -1;

            List<string> upperNew = new List<string>();

            string[] upperFilters = (string[])Registry.GetValue(cameraClass, "UpperFilters", new string[] { "Error" });

            for (int i = 0; i < upperFilters.Length; i++)
            {
                if (upperFilters[i].Equals("ccfltr")) ccfltr = i;
                if (upperFilters[i].Equals("ksthunk")) ksthunk = i;

                upperNew.Add(upperFilters[i]);
            }

            if (ksthunk > -1)
            {
                if (ksthunk < ccfltr)
                {
                    string tmp = upperNew[ccfltr];
                    upperNew[ccfltr] = upperNew[ksthunk];
                    upperNew[ksthunk] = tmp;

                    string[] upperNewArray = upperNew.ToArray();

                    Registry.SetValue(cameraClass, "UpperFilters", upperNewArray);
                    result = true;
                }
            }

            return result;
        }

        private static bool RemoveUpperFilterEntry()
        {
            bool result = false;
            string cameraClass = @"HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Class\{6bdd1fc6-810f-11d0-bec7-08002be2092f}";

            string[] upperFilters = (string[])Registry.GetValue(cameraClass, "UpperFilters", new string[] { "Error" });
            List<string> upperList = upperFilters.ToList<string>();

            var fltrIdx = upperList.FindIndex(s => s.Equals("ccfltr"));

            if (fltrIdx > -1)
            {
                upperList.RemoveAt(fltrIdx);
                result = true;
            }

            string[] upperNew = upperList.ToArray();
            Registry.SetValue(cameraClass, "UpperFilters", upperNew);

            return result;
        }

        static void Main(string[] args)
        {
            bool supported = false;

            if (args.Length > 0)
            {
                for (int idx = 0; idx < args.Length; idx++)
                {
                    if (args[idx].Equals("/install"))
                    {
                        supported = true;

                        if (IsAdmin())
                        {
                            if (File.Exists(AppDomain.CurrentDomain.BaseDirectory + "ccfltr.inf"))
                            {
                                ProcessStartInfo start = new ProcessStartInfo();
                                start.Arguments = @"setupapi.dll,InstallHinfSection DefaultInstall 128 " +
                                    AppDomain.CurrentDomain.BaseDirectory + "ccfltr.inf";
                                start.FileName = Environment.SystemDirectory + @"\rundll32.exe";
                                start.WindowStyle = ProcessWindowStyle.Normal;

                                try
                                {
                                    var p = Process.Start(start);
                                    p.WaitForExit();

                                    Console.WriteLine("Success: ccfltr installed.");

                                    if (SetupUpperFilters())
                                    {
                                        Console.WriteLine("Success: ccfltr set before ksthunk.");
                                    }
                                }
                                catch (Exception e)
                                {
                                    Console.WriteLine(e.Message);
                                    Console.WriteLine(e.StackTrace);
                                }
                            }
                            else
                            {
                                Console.WriteLine("Cannot find ccfltr.inf file.");
                            }
                        }
                        else
                        {
                            Console.WriteLine("Not running on an elevated command prompt.");
                        }

                        break;
                    }

                    if (args[idx].Equals("/uninstall"))
                    {
                        supported = true;

                        if (IsAdmin())
                        {
                            if (File.Exists(AppDomain.CurrentDomain.BaseDirectory + "ccfltr.inf"))
                            {
                                ProcessStartInfo start = new ProcessStartInfo();
                                start.Arguments = @"setupapi.dll,InstallHinfSection DefaultUninstall 128 " +
                                    AppDomain.CurrentDomain.BaseDirectory + "ccfltr.inf";
                                start.FileName = Environment.SystemDirectory + @"\rundll32.exe";
                                start.WindowStyle = ProcessWindowStyle.Normal;

                                try
                                {
                                    var p = Process.Start(start);
                                    p.WaitForExit();

                                    Console.WriteLine("Success: ccfltr uninstalled.");

                                    if (RemoveUpperFilterEntry())
                                    {
                                        Console.WriteLine("Success: ccfltr removed from filter list.");
                                    }
                                }
                                catch (Exception e)
                                {
                                    Console.WriteLine(e.Message);
                                    Console.WriteLine(e.StackTrace);
                                }
                            }
                            else
                            {
                                Console.WriteLine("Cannot find ccfltr.inf file.");
                            }
                        }
                        else
                        {
                            Console.WriteLine("Not running on an elevated command prompt.");
                        }

                        break;
                    }
                }
            }

            if (!supported) Help();
        }
    }
}
