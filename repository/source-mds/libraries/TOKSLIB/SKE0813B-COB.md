# SKE0813B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0813B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品システム                  *
*    業務名　　　　　　　：　出荷検品                          *
*    モジュール名　　　　：　検品送信管理Ｆ更新                *
*    作成日／更新日　　　：　01/04/17                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　検品送信管理Ｆへ処理結果・異常個所*
*                            ＣＤを登録する。                 *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKE0813B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         01/04/17.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*検品送信管理Ｆ
     SELECT   KENSNDF  ASSIGN    TO        DA-01-VI-KENSNDL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                       FILE  STATUS    IS  KEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
*    検品送信管理ファイル
******************************************************************
 FD  KENSNDF           LABEL     RECORD    IS   STANDARD.
                       COPY      KENSNDF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  KENSNDF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEN-ERR           PIC N(15) VALUE
         NC"検品送信管理Ｆエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(04).
     03  FILLER         PIC  9(04).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE               SECTION.
 01  PARA-PDATE             PIC   9(08).
 01  PARA-PTIME             PIC   9(04).
 01  PARA-KEKA              PIC   X(01).
 01  PARA-SKEKA             PIC   X(02).
 01  PARA-FIL1-CNT          PIC   9(06).
 01  PARA-FIL2-CNT          PIC   9(06).
**************************************************************
 PROCEDURE             DIVISION USING PARA-PDATE
                                      PARA-PTIME
                                      PARA-KEKA
                                      PARA-SKEKA
                                      PARA-FIL1-CNT
                                      PARA-FIL2-CNT.
**************************************************************
 DECLARATIVES.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KENSNDF.
     MOVE        KEN-ST    TO        E-ST.
     MOVE       "KENSNDL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O     KENSNDF.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*日付・時間で検品送信管理Ｆ存在チェック
     MOVE      PARA-PDATE    TO   KEN-F01.
     MOVE      PARA-PTIME    TO   KEN-F02.
     READ      KENSNDF
               INVALID  MOVE  "INV"  TO  KENSNDF-INV-FLG
           NOT INVALID  MOVE  SPACE  TO  KENSNDF-INV-FLG
     END-READ.
*更新チェック
     IF        KENSNDF-INV-FLG = "INV"
               MOVE   SPACE        TO   KEN-REC
               INITIALIZE               KEN-REC
               MOVE   PARA-PDATE   TO   KEN-F01
               MOVE   PARA-PTIME   TO   KEN-F02
               MOVE   PARA-KEKA    TO   KEN-F03
               MOVE   PARA-SKEKA   TO   KEN-F04
               MOVE   PARA-FIL1-CNT TO  KEN-F05
               MOVE   PARA-FIL2-CNT TO  KEN-F06
               WRITE  KEN-REC
     ELSE
               MOVE   PARA-KEKA    TO   KEN-F03
               MOVE   PARA-SKEKA   TO   KEN-F04
               MOVE   PARA-FIL1-CNT TO  KEN-F05
               MOVE   PARA-FIL2-CNT TO  KEN-F06
               REWRITE  KEN-REC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     KENSNDF.
*
 END-EXIT.
     EXIT.
*****************<<  SKE0813B   END PROGRAM  >>******************

```
