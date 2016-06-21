# BeRoTinyPascal
A self-hosting capable tiny pascal compiler for the Win32 x86 platform 

## License

    ******************************************************************************
    *                                zlib license                                *
    *============================================================================*
    *                                                                            *
    * Copyright (C) 2006-2016, Benjamin Rosseaux (benjamin@rosseaux.com)         *
    *                                                                            *
    * This software is provided 'as-is', without any express or implied          *
    * warranty. In no event will the authors be held liable for any damages      *
    * arising from the use of this software.                                     *
    *                                                                            *
    * Permission is granted to anyone to use this software for any purpose,      *
    * including commercial applications, and to alter it and redistribute it     *
    * freely, subject to the following restrictions:                             *
    *                                                                            *
    * 1. The origin of this software must not be misrepresented; you must not    *
    *    claim that you wrote the original software. If you use this software    *
    *    in a product, an acknowledgement in the product documentation would be  *
    *    appreciated but is not required.                                        *
    * 2. Altered source versions must be plainly marked as such, and must not be *
    *    misrepresented as being the original software.                          *
    * 3. This notice may not be removed or altered from any source distribution. *
    *                                                                            *
    ******************************************************************************
    
## General guidelines for code contributors

1. Make sure you are legally allowed to make a contribution under the zlib license.                                                   
2. The zlib license header goes at the top of each source file, with appropriate copyright notice.
3. After a pull request, check the status of your pull request on http://github.com/BeRo1985/berotinypascal .
4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 and even with BeRoTinyPascal itself, so don't use generics/templates, operator overloading and another newer syntax features than Delphi 7 and BeRoTinyPascal have support for that     
5. Don't use any libraries/units except the RTL system unit functions.
6.  Make sure the code compiles with Delphi 7, FreePascal >= 3.0 and with BeRoTinyPascal itself.
