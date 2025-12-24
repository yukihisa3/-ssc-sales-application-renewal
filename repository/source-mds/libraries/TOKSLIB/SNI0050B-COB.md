# SNI0050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SNI0050B.COB`

## ソースコード

```cobol
************************************************************
*   顧客名　　    ：   _サカタのタネ殿                    *
*   システム名    ：   在庫管理システム                    *
*   サブシステム名：   日次処理　                          *
*   プログラム名　：   仕入計上データカウント              *
*   プログラムID  ：   SNI0050B                            *
*   作成者        ：   ＮＡＶ　                            *
*   作成日        ：   1993.05.03                          *
*   更新日        ：                                       *
************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SNI0050B.
 AUTHOR.                NAV.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   YB
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 振替ファイル         >>******************************
     SELECT   INFILE    ASSIGN  TO   DA-01-S-TOKU
                        ACCESS  MODE         IS   SEQUENTIAL
                        FILE    STATUS       IS   IN-STATUS.
****<<  プリントファイル    >>******************************
     SELECT   PRINTF    ASSIGN  TO   LP-04.
***
 DATA                   DIVISION.
 FILE                   SECTION.
****<< 振替ファイル         >>******************************
 FD    INFILE.
*
 01  IN-REC.
     03  IN-F01                  PIC  9(04).
     03  IN-F02                  PIC  9(02).
     03  IN-F03                  PIC  X(09).
     03  IN-F04                  PIC  9(02).
     03  IN-F05                  PIC  9(01).
     03  IN-F06                  PIC  X(08).
     03  IN-F07                  PIC  9(05).
     03  IN-F08                  PIC  9(08).
     03  IN-F09                  PIC  9(08).
     03  IN-F10                  PIC  9(08).
     03  IN-F11                  PIC  X(08).
     03  IN-F12                  PIC  X(08).
     03  IN-F13                  PIC  X(05).
     03  IN-F14                  PIC  X(01).
     03  IN-F15                  PIC  X(01).
     03  IN-F16                  PIC S9(07)V99.
     03  IN-F17                  PIC S9(07)V99.
     03  IN-F18                  PIC S9(09).
     03  IN-F19                  PIC  X(02).
     03  IN-F20                  PIC  X(01).
     03  IN-F21                  PIC  X(01).
     03  IN-F22                  PIC  X(02).
     03  IN-F23                  PIC  X(10).
     03  IN-F24                  PIC  X(07).
     03  IN-F25                  PIC  X(13).
     03  IN-F26                  PIC  9(02).
     03  IN-F27                  PIC  9(01).
     03  IN-F28                  PIC  9(08).
     03  IN-F29                  PIC  X(08).
     03  IN-F30                  PIC  X(01).
     03  IN-F31                  PIC S9(07)V99.
     03  IN-F32                  PIC S9(09).
     03  IN-F33                  PIC  X(06).
     03  IN-F34                  PIC  X(09).
     03  IN-F90                  PIC  X(62).
*
****<<  プリントファイル  >>********************************
 FD    PRINTF.
 01    PRT-REC               PIC  X(200).
****************************************************************
 WORKING-STORAGE        SECTION.
*
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 IN-STATUS            PIC  X(02).
****  フラグ                  ****
 01  PSW-AREA.
     02  END-FLG             PIC  9(01)  VALUE  ZERO.
****  日付保存                ****
 01  SYSYMD                  PIC  9(06).
 01  SYSYMD-R                REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
****  カウンタ                ****
 01  CNT-AREA.
     02  L-CNT                   PIC  9(02)  VALUE  62.
     02  P-CNT                   PIC  9(03)  VALUE  ZERO.
     02  READ-CNT1               PIC  9(07)  VALUE  ZERO.
     02  READ-CNT2               PIC  9(07)  VALUE  ZERO.
     02  WRITE-CNT               PIC  9(07).
     02  SKIP-CNT                PIC  9(07).
****  見出し行１             ****
 01  MIDASI-1.
     03  FILLER              PIC  X(34)  VALUE  SPACE.
     03  FILLER              PIC  N(16)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊＊　在庫データリスト　＊＊＊".
     03  FILLER              PIC  X(19)  VALUE  SPACE.
     03  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  YA         VALUE  NC"処理日".
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  H-YY                PIC  99.
     03  FILLER              PIC  X(1)   VALUE  ".".
     03  H-MM                PIC  Z9.
     03  FILLER              PIC  X(1)   VALUE  ".".
     03  H-DD                PIC  Z9.
     03  FILLER              PIC  X(3)   VALUE  SPACE.
     03  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  YA         VALUE  NC"頁".
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  PAGE-SUU            PIC  ZZ9.
****  明細行　　　           ****
 01  MEISAI1        CHARACTER     TYPE   IS   YA.
     03  FILLER              PIC  X(35)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"仕入計上".
     03  FILLER              PIC  N(05)  VALUE  NC"データ件数".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  KENSU1              PIC  ---,--9.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"件".
 01  MEISAI2        CHARACTER     TYPE   IS   YA.
     03  FILLER              PIC  X(35)  VALUE  SPACE.
     03  FILLER              PIC  N(04)  VALUE  NC"入出庫　".
     03  FILLER              PIC  N(05)  VALUE  NC"データ件数".
     03  FILLER              PIC  X(05)  VALUE  SPACE.
     03  KENSU2              PIC  ---,--9.
     03  FILLER              PIC  X(03)  VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"件".
*
************************************************************
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
************************************************************
*
 PROCEDURE              DIVISION.
*
 GENERAL-PROCESS        SECTION.
     OPEN    INPUT     INFILE
             OUTPUT    PRINTF.
*
     ACCEPT   SYSYMD    FROM   DATE.
     MOVE     SYS-YY    TO     H-YY.
     MOVE     SYS-MM    TO     H-MM.
     MOVE     SYS-DD    TO     H-DD.
*
     PERFORM       UNTIL     END-FLG  =    9
             READ  INFILE
                   AT END    MOVE     9    TO   END-FLG
                   NOT AT END
                      IF     IN-F02   =    "31" OR   "32" OR
                                           "30"
                             ADD      1    TO   READ-CNT2
                      ELSE
                             ADD      1    TO   READ-CNT1
                      END-IF
             END-READ
     END-PERFORM.
*
     ADD      1  TO     P-CNT.
     MOVE     P-CNT     TO    PAGE-SUU.
     WRITE    PRT-REC FROM    MIDASI-1.
*
     MOVE     READ-CNT1 TO    KENSU1.
     MOVE     READ-CNT2 TO    KENSU2.
     WRITE    PRT-REC FROM    MEISAI1 AFTER 20.
     WRITE    PRT-REC FROM    MEISAI2 AFTER 2.
*
     CLOSE             INFILE
                       PRINTF.
*****************<<  PROGRAM  END  >>***********************

```
