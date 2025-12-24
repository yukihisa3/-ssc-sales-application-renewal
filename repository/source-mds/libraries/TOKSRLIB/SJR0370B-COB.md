# SJR0370B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0370B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　      *
*    業務名　　　　　　　：　伝票纏め返品　　　　　            *
*    モジュール名　　　　：　返品累積伝票纏め処理　　　　　　　*
*    作成日／更新日　　　：　2017/08/29                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　パラメタを受取り、伝票纏め対象の　*
*                          取引先の伝票の纏め処理を行なう。    *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0370B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2017/08/29.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*返品累積データ
     SELECT  COMRHEF   ASSIGN    TO        COMRHEL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       RHE-F78
                                           RHE-F79
                                           RHE-F01
                                           RHE-F02
                                           RHE-F03
                                           RHE-F04
                                           RHE-F05
                       FILE      STATUS    RHE-ST.
*伝票纏め返品累積データ
     SELECT  COMRMHF   ASSIGN    TO        COMRMHL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       RMH-F01
                                           RMH-F07
                                           RMH-F02
                                           RMH-F04
                                           RMH-F05
                       FILE      STATUS    RMH-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 返品累積データ                                     *
****************************************************************
 FD  COMRHEF
                       BLOCK     CONTAINS  11   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      COMRHEF   OF   XFDLIB
                       JOINING   RHE       AS   PREFIX.
****************************************************************
*    FILE = 伝票纏め返品累積データ                             *
****************************************************************
 FD  COMRMHF
                       BLOCK     CONTAINS  11   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      COMRMHF   OF   XFDLIB
                       JOINING   RMH       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TOK-ST                   PIC  X(02).
     03  RHE-ST                   PIC  X(02).
     03  RMH-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SONZAI-CHK               PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  COMRMHF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
     03  RD-CNT                   PIC  9(07)  VALUE  ZERO.
     03  WT-CNT                   PIC  9(07)  VALUE  ZERO.
     03  WK-TENPO                 PIC  9(05)  VALUE  ZERO.
     03  WK-KENDT                 PIC  9(08)  VALUE  ZERO.
     03  WK-DENNO                 PIC  9(09)  VALUE  ZERO.
     03  WK-GYOCNT                PIC  9(02)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  RHE-ERR           PIC N(20) VALUE
                        NC"返品累積データエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  RMH-ERR           PIC N(20) VALUE
                        NC"伝票纏め返品累積データエラー".
*
 01  WK-DATE-ARE.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE               SECTION.
 01  PARA-BUMON            PIC X(04).
 01  PARA-TANCD            PIC X(02).
 01  PARA-HIDUKE           PIC 9(08).
 01  PARA-JIKAN            PIC 9(04).
 01  PARA-TORCD            PIC 9(08).
*
**************************************************************
**************************************************************
 PROCEDURE             DIVISION  USING
                                 PARA-BUMON
                                 PARA-TANCD
                                 PARA-HIDUKE
                                 PARA-JIKAN
                                 PARA-TORCD.
**************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RHE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE COMRHEF.
     DISPLAY     RHE-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RHE-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RMH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE COMRMHF.
     DISPLAY     RMH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RMH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       COMRHEF  COMRMHF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*伝票累積データスタート
     MOVE     SPACE               TO   RHE-REC.
     INITIALIZE                        RHE-REC.
*
     MOVE  PARA-HIDUKE            TO   RHE-F78.
     MOVE  PARA-JIKAN             TO   RHE-F79.
     MOVE  PARA-TORCD             TO   RHE-F01.
     START COMRHEF KEY IS >= RHE-F78  RHE-F79  RHE-F01
                             RHE-F02  RHE-F03  RHE-F04
                             RHE-F05
           INVALID
           DISPLAY NC"＃対象データ無し１！！＃＃" UPON CONS
           MOVE    "END"          TO   END-FLG
           GO                     TO   INIT-EXIT
     END-START.
*
     PERFORM  COMRHEF-READ-SEC.
     IF  END-FLG = "END"
         DISPLAY NC"＃対象データ無し２！！＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     IF  WK-TENPO  =  RHE-F02
     AND WK-KENDT  =  RHE-F03
         CONTINUE
     ELSE
         MOVE   RHE-F02      TO   WK-TENPO
         MOVE   RHE-F03      TO   WK-KENDT
         MOVE   RHE-F04      TO   WK-DENNO
         MOVE   ZERO         TO   WK-GYOCNT
     END-IF.
*
     ADD        1            TO   WK-GYOCNT.
     IF  WK-GYOCNT  >  6
         MOVE   RHE-F04      TO   WK-DENNO
         MOVE   1            TO   WK-GYOCNT
     END-IF.
*レコードセット
     MOVE       SPACE        TO   RMH-REC.
     INITIALIZE                   RMH-REC.
*
     MOVE       RHE-REC      TO   RMH-REC.
     MOVE       WK-DENNO     TO   RMH-F04.
     MOVE       WK-GYOCNT    TO   RMH-F05.
     MOVE       RHE-F04      TO   RMH-F28.
     MOVE       RHE-F05      TO   RMH-F29.
     MOVE       ZERO         TO   RMH-F30  RMH-F31.
     MOVE       SPACE        TO   RMH-F32  RMH-F33.
     MOVE       "1"          TO   RMH-F27.
     MOVE       ZERO         TO   RMH-F96  RMH-F97.
     MOVE       SPACE        TO   RMH-F98  RMH-F99.
     MOVE       PARA-BUMON   TO   RMH-F94.
     MOVE       PARA-TANCD   TO   RMH-F95.
     MOVE       SYS-DATEW    TO   RMH-F92.
     MOVE       WK-TIME(1:6) TO   RMH-F93.
     MOVE       RHE-F04      TO   RMH-F21.
*
     MOVE       "1"          TO   RHE-F27.
     MOVE       WK-DENNO     TO   RHE-F28.
     MOVE       WK-GYOCNT    TO   RHE-F29.
     MOVE       SYS-DATEW    TO   RHE-F30.
     MOVE       WK-TIME(1:6) TO   RHE-F31.
     MOVE       PARA-BUMON   TO   RHE-F32.
     MOVE       PARA-TANCD   TO   RHE-F33.
     MOVE       "1"          TO   RHE-F80.
     MOVE       RHE-F23      TO   RMH-F23.
*
     PERFORM  COMRMHF-READ-SEC.
     IF  COMRMHF-INV-FLG  = "INV"
         WRITE  RMH-REC
         ADD    1            TO   WT-CNT
         REWRITE RHE-REC
     ELSE
         DISPLAY NC"＃纏め返品Ｄ作成エラー！＃" UPON CONS
         DISPLAY "#RMH-F02 = " RMH-F02  " #" UPON CONS
         DISPLAY "#RMH-F03 = " RMH-F03  " #" UPON CONS
         DISPLAY "#RMH-F04 = " RMH-F04  " #" UPON CONS
         DISPLAY "#RMH-F05 = " RMH-F05  " #" UPON CONS
         MOVE    4000        TO   PROGRAM-STATUS
         STOP  RUN
     END-IF.
*
     PERFORM  COMRHEF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    返品累積データ読込
****************************************************************
 COMRHEF-READ-SEC     SECTION.
     MOVE     "COMRHEF-READ-SEC" TO   S-NAME.
*
     READ  COMRHEF
           AT  END      MOVE  "END"   TO   END-FLG
                        GO            TO   COMRHEF-READ-EXIT
     END-READ.
*
     ADD   1                          TO   RD-CNT.
     IF  RD-CNT(5:3)  =  "000"  OR  "500"
         DISPLAY "## RD-CNT = " RD-CNT  UPON CONS
     END-IF.
*
*****DISPLAY "PARA-HIDUKE = " PARA-HIDUKE UPON CONS.
*    DISPLAY "PARA-JIKAN  = " PARA-JIKAN  UPON CONS.
*    DISPLAY "PARA-TORCD  = " PARA-TORCD  UPON CONS.
*    DISPLAY "RHE-F78     = " RHE-F78     UPON CONS.
*    DISPLAY "RHE-F79     = " RHE-F79     UPON CONS.
*****DISPLAY "RHE-F01     = " RHE-F01     UPON CONS.
     IF  PARA-HIDUKE  =  RHE-F78
     AND PARA-JIKAN   =  RHE-F79
     AND PARA-TORCD   =  RHE-F01
         CONTINUE
     ELSE
         MOVE  "END"             TO   END-FLG
         GO                      TO   COMRHEF-READ-EXIT
     END-IF.
*取引先マスタ索引
     MOVE    RHE-F01             TO   TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = "INV"
         GO                      TO   COMRHEF-READ-SEC
     ELSE
         IF  TOK-FIL1(1:1)  =  SPACE
             GO                  TO   COMRHEF-READ-SEC
         END-IF
     END-IF.
*伝票纏め済の場合
     IF  RHE-F27  =  "1"
         GO                      TO   COMRHEF-READ-SEC
     END-IF.
*
 COMRHEF-READ-EXIT.
     EXIT.
****************************************************************
*  　伝票纏め返品累積データ読込　                              *
****************************************************************
 COMRMHF-READ-SEC      SECTION.
*
     MOVE     "COMRMHF-READ-SEC"  TO   S-NAME.
*
     READ  COMRMHF
           INVALID     MOVE  "INV"   TO   COMRMHF-INV-FLG
           NOT INVALID MOVE  SPACE   TO   COMRMHF-INV-FLG
     END-READ.
*
 COMRMHF-READ-EXIT.
     EXIT.
****************************************************************
*  　取引先マスタ読込　　　　　　                              *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ  HTOKMS
           INVALID     MOVE  "INV"   TO   HTOKMS-INV-FLG
           NOT INVALID MOVE  SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*  　終了処理　　　　　　　　　　                              *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*
     DISPLAY NC"読込件数　　" " = " RD-CNT UPON CONS.
     DISPLAY NC"纏め伝票件数" " = " WT-CNT UPON CONS.
*
     CLOSE  HTOKMS  COMRHEF  COMRMHF.
*
     STOP  RUN.
*
 END-EXIT.
     EXIT.
*****************<<  SJR0370B   END PROGRAM  >>******************

```
