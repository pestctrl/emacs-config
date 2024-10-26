;;; my-clang-options.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2024-10-25 14:44]

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'clang-command)

(register-prebaked-optionset clang-subtargets "ARM" cortex-r4-soft-float
  :target "-target arm -mcpu=cortex-r4 -mfpu=none -mfloat-abi=soft -mlittle-endian")

(register-prebaked-optionset clang-subtargets "ARM" cortex-hard-float
  :target "-target arm -mcpu=cortex-r4 -mfpu=vfpv3-d16 -mfloat-abi=hard -mlittle-endian")

(register-prebaked-optionset clang-subtargets "ARM" cortex-hard-float-tmu
  :target "-target arm -mcpu=cortex-r4 -mfpu=vfpv3-d16 -mfloat-abi=hard -mlittle-endian")

(register-prebaked-optionset clang-subtargets "ARM" cortex-m0
  :target "-target arm -mcpu=cortex-m0 -mlittle-endian -mthumb")

(register-prebaked-optionset clang-subtargets "ARM" cortex-m4
  :target "-target arm -mcpu=cortex-m4 -mfloat-abi=hard -mfpu=fpv4-sp-d16")

(register-prebaked-optionset clang-subtargets "ARM" cortex-m33
  :target "-target arm -mcpu=cortex-m33 -mfpu=vfpv3-d16 -mfloat-abi=hard -mlittle-endian")

(register-prebaked-optionset clang-subtargets "RISCV" rv32im
  :target "-target riscv32-unknown-elf -march=rv32im -mabi=ilp32"
  :optimization "-O3")

(register-prebaked-optionset clang-subtargets "RISCV" rv32imafc
  :target "-target riscv32-unknown-elf -march=rv32imafc -mabi=ilp32f -mno-relax"
  :optimization "-O3")

(provide 'my-clang-options)
;;; my-clang-options.el ends here
