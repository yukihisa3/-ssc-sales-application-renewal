# TWAIT180

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/TWAIT180.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　自動受信サブシシテム　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            TWAIT180.
 DATE-WRITTEN.          93/12/02.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS.
***************************************************************
 INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 WORKING-STORAGE           SECTION.
*--------------------------------------------------------------*
 01  TIMEX                   PIC  9(08).
 01  XTWAIT-TIME             PIC  9(06)  BINARY.
 01  XTWAIT-WORK             PIC  X(64).
*
*
******************************************************************
 PROCEDURE                 DIVISION.
******************************************************************
     ACCEPT       TIMEX      FROM        TIME.
     DISPLAY      TIMEX      UPON        CONS.
     MOVE         180        TO          XTWAIT-TIME.
     CALL         "XSBTWAIT" USING       XTWAIT-TIME
                                         XTWAIT-WORK.
     ACCEPT       TIMEX      FROM        TIME.
     DISPLAY      TIMEX      UPON        CONS.
*****STOP         RUN.
*----------------------------------------------------------------*

```
