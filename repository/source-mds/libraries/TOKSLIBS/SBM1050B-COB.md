# SBM1050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM1050B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳ                  *
*    モジュール名　　　　：　変換結果更新　　　　　　　　　　  *
*    作成日／更新日　　　：　12/11/22                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　イオン売上フォーマット変換の結果を*
*                            イオン取引先受信ワークに更新する。*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM1050B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/11/22.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   YA
     YB           IS   YB
     YA-22        IS   YA-22
     YB-21        IS   YB-21
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*イオン取引先受信ワーク
     SELECT  IONTWKF      ASSIGN    TO     DA-01-VI-IONTWKL1
                          ORGANIZATION     INDEXED
                          ACCESS    MODE   RANDOM
                          RECORD    KEY    TWK-F01
                                           TWK-F02
                                           TWK-F03
                                           TWK-F04
                          FILE      STATUS TWK-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = イオン取引先受信ワーク                             *
****************************************************************
 FD  IONTWKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      IONTWKF   OF   XFDLIB
                       JOINING   TWK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TWK-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TWK-ERR           PIC N(15) VALUE
         NC"イオン取引先受信ワークエラー".
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
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  END-FLG           PIC X(03) VALUE     SPACE.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  SYSDATE           PIC 9(06) VALUE     ZERO.
     03  IX                PIC 9(02) VALUE     ZERO.
     03  IONTWKF-INV-FLG   PIC X(03) VALUE     SPACE.
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
 01  WK-SYS-TIME             PIC  9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-ERR              PIC X(01).
 01  LINK-JDATE            PIC 9(08).
 01  LINK-JTIME            PIC 9(04).
 01  LINK-TORICD           PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-ERR
                                           LINK-JDATE
                                           LINK-JTIME
                                           LINK-TORICD.
**************************************************************
 DECLARATIVES.
 TWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE IONTWKF.
     MOVE        TWK-ST    TO        E-ST.
     MOVE       "IONTWKL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TWK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃イオン売上変換更新　異常＃" UPON CONS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     DISPLAY NC"＃イオン売上変換更新　開始＃" UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     DISPLAY NC"＃イオン売上変換更新　終了＃" UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O    IONTWKF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*    データ検索
     MOVE      "1"           TO   TWK-F01.
     MOVE      LINK-JDATE    TO   TWK-F02.
     MOVE      LINK-JTIME    TO   TWK-F03.
     MOVE      LINK-TORICD   TO   TWK-F04.
*
     PERFORM  IONTWKF-READ-SEC.
*
     IF  IONTWKF-INV-FLG = SPACE
         MOVE  LINK-ERR      TO   TWK-F06
         REWRITE  TWK-REC
     ELSE
         DISPLAY NC"＃イオン売上変換結果更新エラー＃" UPON CONS
         DISPLAY "# JDATE = " LINK-JDATE " ="  UPON CONS
         DISPLAY "# JTIME = " LINK-JTIME "     ="  UPON CONS
         DISPLAY "# TORCD = " LINK-TORICD " ="  UPON CONS
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    イオン取引先受信ワーク読込                      ALL       *
****************************************************************
 IONTWKF-READ-SEC      SECTION.
     MOVE "IONTWKF-READ-SEC" TO   S-NAME.
*
     READ  IONTWKF
           INVALID      MOVE "INV"  TO  IONTWKF-INV-FLG
           NOT  INVALID MOVE SPACE  TO  IONTWKF-INV-FLG
     END-READ.
*
 IONTWKF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     IONTWKF.
*
 END-EXIT.
     EXIT.
*****************<<  SBM1050B   END PROGRAM  >>******************

```
