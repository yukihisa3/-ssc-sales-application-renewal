# NKE0270L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0270L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　入荷検品　　　　　　　            *
*    モジュール名　　　　：　倉庫別不良品リスト発行　　　　　　*
*    処理概要　　　　　　：　倉庫不良品累積ファイルより、　　　*
*                            条件に合致するリストを出力する。　*
*    流用　　　　　　　　：　NKE0260L                          *
*    作成日／作成者　　　：　2019/01/28 INOUE                  *
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0270L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/01/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*倉庫不良品累積ファイル
     SELECT   SKFRRKL2  ASSIGN    TO       DA-01-VI-SKFRRKL2
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     SEQUENTIAL
                        RECORD    KEY      RUI-F01
                                           RUI-F08
                                           RUI-F02
                                           RUI-F05
                        FILE      STATUS   RUI-STATUS.
*倉庫マスタ
     SELECT     ZSOKMS1 ASSIGN    TO       DA-01-VI-ZSOKMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      SOK-F01
                        FILE      STATUS   SOK-STATUS.
*仕入先マスタ
     SELECT     ZSHIMS1 ASSIGN    TO       DA-01-VI-ZSHIMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      SHI-F01
                        FILE      STATUS   SHI-STATUS.
*条件ファイル
     SELECT     JYOKEN1 ASSIGN    TO       DA-01-VI-JYOKEN1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      JYO-F01  JYO-F02
                        FILE      STATUS   JYO-STATUS.
*商品名称マスタ
     SELECT     MEIMS1  ASSIGN    TO       DA-01-VI-MEIMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      MEI-F011  MEI-F0121
                                           MEI-F0122 MEI-F0123
                        FILE      STATUS   MEI-STATUS.
*担当者マスタ
     SELECT     TANMS1  ASSIGN    TO       DA-01-VI-TANMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TAN-F01  TAN-F02
                        FILE      STATUS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*倉庫不良品累積ファイル
 FD  SKFRRKL2            LABEL RECORD   IS   STANDARD.
     COPY     SKFRRKL2   OF        XFDLIB
     JOINING  RUI       PREFIX.
*倉庫マスタ
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
     JOINING  SOK       AS        PREFIX.
*仕入先マスタ
 FD  ZSHIMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSHIMS1   OF        XFDLIB
     JOINING  SHI       AS        PREFIX.
*条件ファイル
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
     JOINING  JYO       AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
     JOINING  MEI       AS        PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  SKFRRKL2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  SKFRRKL2-ERR-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRINT-OUT-CNT           PIC  9(07)     VALUE  ZERO.
 01  ZSOKMS1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  ZSHIMS1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  JYOKEN1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  WK-SUURYO               PIC S9(09)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  RUI-STATUS        PIC  X(02).
     03  SOK-STATUS        PIC  X(02).
     03  SHI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*BRK項目　エリア
*   仕入先ＣＤ
     03  BRK-RUI-F08       PIC  9(08)    VALUE ZERO.
     03  BRK-RUI-F08-FLG   PIC  X(03)    VALUE SPACE.
*   不良品検品日付
     03  BRK-RUI-F02       PIC  9(08)    VALUE ZERO.
     03  BRK-RUI-F02-FLG   PIC  X(03)    VALUE SPACE.
*対象範囲チェックエリア
*   PARA-IN 仕入先ＣＤ・検品日・ＪＡＮＣＤ（ＦＲＯＭ）
     03  WK-PARA-IN-FROM.
         05  WK-PARA-IN-FROM-SHICD       PIC  9(08).
         05  WK-PARA-IN-FROM-KENDATE     PIC  9(08).
         05  WK-PARA-IN-FROM-JANCD       PIC  X(13).
*   PARA-IN 仕入先ＣＤ・検品日・ＪＡＮＣＤ（ＴＯ）
     03  WK-PARA-IN-TO.
         05  WK-PARA-IN-TO-SHICD         PIC  9(08).
         05  WK-PARA-IN-TO-KENDATE       PIC  9(08).
         05  WK-PARA-IN-TO-JANCD         PIC  X(13).
*   DATA 仕入先ＣＤ・検品日・ＪＡＮＣＤ
     03  WK-RUI-KEY.
         05  WK-RUI-KEY-F08              PIC  9(08).
         05  WK-RUI-KEY-F02              PIC  9(08).
         05  WK-RUI-KEY-F05              PIC  X(13).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0270L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0270L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0270L".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD0.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "NKE0270L".
         05  FILLER          PIC  X(33)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA-22.
         05  FILLER          PIC  N(11)     VALUE
         NC"＜倉庫別不良品リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(28)     VALUE  SPACE.
         05  HD0-YYYY        PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD0-MM          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD0-DD          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD0-PCNT        PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD00.
     03  FILLER              PIC  X(116)    VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD00-MS         PIC  9(02).
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"倉　庫：".
         05  HD000-SOKCD     PIC  X(02).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD000-SOKNM     PIC  N(18).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
*
 01  HD0000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"仕入先：".
         05  HD0000-SHICD    PIC  9(08).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD0000-SHINM    PIC  N(18).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
*
 01  SEN.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)  VALUE SPACE.
         05  FILLER          PIC  N(06)  VALUE NC"不良品検品日".
         05  FILLER          PIC  X(03)  VALUE SPACE.
         05  FILLER          PIC  N(05)  VALUE NC"ＪＡＮＣＤ".
         05  FILLER          PIC  X(08)  VALUE SPACE.
         05  FILLER          PIC  N(03)  VALUE NC"商品名".
         05  FILLER          PIC  X(31)  VALUE SPACE.
         05  FILLER          PIC  N(04)  VALUE NC"不良品数".
         05  FILLER          PIC  X(05)  VALUE SPACE.
         05  FILLER          PIC  N(05)  VALUE NC"不良品区分".
         05  FILLER          PIC  X(04)  VALUE SPACE.
         05  FILLER          PIC  X(01)  VALUE "(".
         05  FILLER          PIC  X(04)  VALUE SPACE.
         05  FILLER          PIC  N(06)  VALUE NC"自社商品情報".
         05  FILLER          PIC  X(06)  VALUE SPACE.
         05  FILLER          PIC  X(01)  VALUE ")".
         05  FILLER          PIC  X(02)  VALUE SPACE.
         05  FILLER          PIC  N(06)  VALUE NC"発注伝票情報".
         05  FILLER          PIC  X(02)  VALUE SPACE.
         05  FILLER          PIC  N(08)
                                  VALUE NC"不良品検品担当者".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KENDATE    PIC  X(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-JANCD      PIC  X(13).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SYONAME    PIC  N(20).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-FSURYOU    PIC  ZZ,ZZZ,ZZ9.99.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-FKBNCD     PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-FKBNNM     PIC  N(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SYOCD      PIC  X(21).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(07).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  MS01-GYONO      PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANCD      PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANNM      PIC  N(06).
*
*01  MS03.
*    03  FILLER              CHARACTER TYPE YB.
*        05  FILLER          PIC  X(23)     VALUE  SPACE.
*        05  MS03-SYONAME    PIC  N(20).
*        05  FILLER          PIC  X(04)     VALUE  SPACE.
*        05  MS03-HACSU      PIC  ZZZ,ZZ9.
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  MS03-ZANSU      PIC  ZZZ,ZZ9.
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  MS03-NYUSU      PIC  ZZZ,ZZ9.
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  FILLER          PIC  X(01)     VALUE  "(".
*        05  MS03-FURYOSU    PIC  ZZZ,ZZ9.
*        05  FILLER          PIC  X(01)     VALUE  ")".
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*        05  MS03-FURYOKBN   PIC  X(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  MS03-FURYONM    PIC  N(10).
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*    03  FILLER              CHARACTER TYPE YA.
*        05  MS03-ERR1       PIC  N(01).
*        05  MS03-ERR2       PIC  N(01).
*        05  MS03-ERR3       PIC  N(01).
*        05  MS03-ERR4       PIC  N(01).
*        05  MS03-ERR5       PIC  N(01).
*        05  MS03-ERR6       PIC  N(01).
*        05  MS03-ERR7       PIC  N(01).
*        05  MS03-ERR8       PIC  N(01).
*        05  MS03-ERR9       PIC  N(01).
*        05  MS03-ERR10      PIC  N(01).
*
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　対象データは０件です　！！　　　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
 01  PARA-IN-SOKCD         PIC   X(02).
 01  PARA-IN-SHICDF        PIC   9(08).
 01  PARA-IN-SHICDT        PIC   9(08).
 01  PARA-IN-KENDATEF      PIC   9(08).
 01  PARA-IN-KENDATET      PIC   9(08).
 01  PARA-IN-JANCDF        PIC   X(13).
 01  PARA-IN-JANCDT        PIC   X(13).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION
                                 USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-SOKCD
                                       PARA-IN-SHICDF
                                       PARA-IN-SHICDT
                                       PARA-IN-KENDATEF
                                       PARA-IN-KENDATET
                                       PARA-IN-JANCDF
                                       PARA-IN-JANCDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SKFRRKL2.
     MOVE      "SKFRRKL2"    TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS1.
     MOVE      "ZSOKMS1 "   TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSHIMS1.
     MOVE      "ZSHIMS1  "   TO   AB-FILE.
     MOVE      SHI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1  "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     SKFRRKL2
                        ZSOKMS1
                        ZSHIMS1
                        JYOKEN1
                        MEIMS1
                        TANMS1.
     OPEN     OUTPUT    PRTF.
*
     DISPLAY  MSG-START UPON CONS.
*
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
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT   SYS-TIME          FROM   TIME.
*
*    仕入先ＣＤ・検品日・ＪＡＮＣＤ　範囲チェック用項目セット
     MOVE     PARA-IN-SHICDF      TO   WK-PARA-IN-FROM-SHICD.
     MOVE     PARA-IN-SHICDT      TO   WK-PARA-IN-TO-SHICD.
     MOVE     PARA-IN-KENDATEF    TO   WK-PARA-IN-FROM-KENDATE.
     MOVE     PARA-IN-KENDATET    TO   WK-PARA-IN-TO-KENDATE.
     MOVE     PARA-IN-JANCDF      TO   WK-PARA-IN-FROM-JANCD.
     MOVE     PARA-IN-JANCDT      TO   WK-PARA-IN-TO-JANCD.
*
*倉庫不良品累積ファイルスタート
     PERFORM  SKFRRKL2-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*倉庫不良品累積ファイル読込
     PERFORM SKFRRKL2-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし．＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*
     MOVE    RUI-F08            TO    BRK-RUI-F08.
     MOVE    RUI-F02            TO    BRK-RUI-F02.
     MOVE    "STR"              TO    BRK-RUI-F02-FLG.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    倉庫不良品累積ファイルスタート
****************************************************************
 SKFRRKL2-START-SEC          SECTION.
*
     MOVE    "SKFRRKL2-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   RUI-REC.
     INITIALIZE                        RUI-REC.
*
     MOVE     PARA-IN-SOKCD       TO   RUI-F01.
     MOVE     PARA-IN-SHICDF      TO   RUI-F08.
     MOVE     PARA-IN-KENDATEF    TO   RUI-F02.
     MOVE     SPACE               TO   RUI-F05.
*
     START    SKFRRKL2  KEY  IS  >=    RUI-F01 RUI-F08
                                       RUI-F02 RUI-F05
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 SKFRRKL2-START-EXIT.
     EXIT.
*
****************************************************************
*    倉庫不良品累積ファイル読込
****************************************************************
 SKFRRKL2-READ-SEC           SECTION.
*
     MOVE    "SKFRRKL2-READ-SEC"   TO   S-NAME.
*
 SKFRRKL2-READ-01.
     READ     SKFRRKL2  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   SKFRRKL2-READ-EXIT
     END-READ.
*条件範囲内判定
*    倉庫ＣＤチェック
     IF       RUI-F01 NOT =  PARA-IN-SOKCD
              MOVE  "END"    TO   END-FLG
              GO             TO   SKFRRKL2-READ-EXIT
     END-IF.
*
*    仕入先ＣＤ・検品日・ＪＡＮＣＤ　範囲チェック
*    MOVE     RUI-F08             TO   WK-RUI-KEY-F08.
*    MOVE     RUI-F02             TO   WK-RUI-KEY-F02.
*    MOVE     RUI-F05             TO   WK-RUI-KEY-F05.
*    IF     ( WK-RUI-KEY          >=   WK-PARA-IN-FROM ) AND
*           ( WK-RUI-KEY          <=   WK-PARA-IN-TO   )
*             CONTINUE
*    ELSE
*             MOVE  "END"    TO   END-FLG
*             GO             TO   SKFRRKL2-READ-EXIT
*    END-IF.
*    仕入先ＣＤ範囲チェック
     IF     ( RUI-F08             >=   PARA-IN-SHICDF  ) AND
            ( RUI-F08             <=   PARA-IN-SHICDT  )
              CONTINUE
     ELSE
              MOVE  "END"    TO   END-FLG
              GO             TO   SKFRRKL2-READ-EXIT
     END-IF.
*    検品日範囲チェック
     IF     ( RUI-F02             >=   PARA-IN-KENDATEF ) AND
            ( RUI-F02             <=   PARA-IN-KENDATET )
              CONTINUE
     ELSE
              GO             TO   SKFRRKL2-READ-01
     END-IF.
*    ＪＡＮＣＤ範囲チェック
     IF     ( RUI-F05             >=   PARA-IN-JANCDF ) AND
            ( RUI-F05             <=   PARA-IN-JANCDT )
              CONTINUE
     ELSE
              GO             TO   SKFRRKL2-READ-01
     END-IF.
*件数カウント
     ADD      1                   TO   SKFRRKL2-READ-CNT.
*
 SKFRRKL2-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     PERFORM  MEISAI-WT-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
     PERFORM  SKFRRKL2-READ-SEC.
*
 MAIN-030.
     IF       RUI-F08  NOT = BRK-RUI-F08
              MOVE     "BRK"     TO   BRK-RUI-F08-FLG
     END-IF.
*
 MAIN-040.
     IF       RUI-F02  NOT = BRK-RUI-F02
              MOVE     "BRK"     TO   BRK-RUI-F02-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD0-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD0-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD0-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD0-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    倉庫名取得
     MOVE     RUI-F01             TO   SOK-F01 HD000-SOKCD.
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = SPACE
              MOVE  SOK-F02       TO   HD000-SOKNM
     ELSE
              MOVE  ALL NC"？"    TO   HD000-SOKNM
     END-IF.
*    仕入先名取得
     MOVE     RUI-F08             TO   SHI-F01 HD0000-SHICD.
     PERFORM  ZSHIMS1-READ-SEC
     IF       ZSHIMS1-INV-FLG = SPACE
              MOVE  SHI-F02       TO   HD0000-SHINM
     ELSE
              MOVE  ALL NC"？"    TO   HD0000-SHINM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD0     AFTER  2.
     WRITE    PRT-REC       FROM  HD00    AFTER  1.
     WRITE    PRT-REC       FROM  HD000   AFTER  1.
     WRITE    PRT-REC       FROM  HD0000  AFTER  1.
     WRITE    PRT-REC       FROM  SEN     AFTER  1.
     WRITE    PRT-REC       FROM  HD01    AFTER  1.
     WRITE    PRT-REC       FROM  SEN     AFTER  1.
     MOVE     SPACE               TO      PRT-REC.
     WRITE    PRT-REC                     AFTER  1.
*行カウントアップ
     MOVE     9                   TO      LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*
*   ブレイク改頁判定
     IF       BRK-RUI-F08-FLG     =   "BRK"
              PERFORM  HEAD-WT-SEC
              MOVE     RUI-F08    TO   BRK-RUI-F08
              MOVE     RUI-F02    TO   BRK-RUI-F02
              MOVE     "   "      TO   BRK-RUI-F08-FLG
              MOVE     "STR"      TO   BRK-RUI-F02-FLG
     END-IF.
*   改頁判定
     IF       LINE-CNT >  50
              PERFORM  HEAD-WT-SEC
              MOVE    "STR"                TO   BRK-RUI-F02-FLG
     END-IF.
*
*明細
     IF     ( BRK-RUI-F02-FLG     =   "STR" ) OR
            ( BRK-RUI-F02-FLG     =   "BRK" )
*    不良品検品日
              MOVE     RUI-F02(1:4)        TO   MS01-KENDATE(1:4)
              MOVE     "/"                 TO   MS01-KENDATE(5:1)
              MOVE     RUI-F02(5:2)        TO   MS01-KENDATE(6:2)
              MOVE     "/"                 TO   MS01-KENDATE(8:1)
              MOVE     RUI-F02(7:2)        TO   MS01-KENDATE(9:2)
              MOVE     "   "               TO   BRK-RUI-F02-FLG
              MOVE     RUI-F02             TO   BRK-RUI-F02
     ELSE
              MOVE     SPACE               TO   MS01-KENDATE
     END-IF.
*    ＪＡＮＣＤ
     MOVE     RUI-F05             TO   MS01-JANCD.
*    商品名
     MOVE     RUI-F06             TO   MEI-F011.
     MOVE     RUI-F071            TO   MEI-F0121.
     MOVE     RUI-F072            TO   MEI-F0122.
     MOVE     RUI-F073            TO   MEI-F0123.
     PERFORM  MEIMS1-READ-SEC.
     IF       MEIMS1-INV-FLG = SPACE
              MOVE  MEI-F02       TO   MS01-SYONAME
     ELSE
              MOVE  ALL NC"？"    TO   MS01-SYONAME
     END-IF
*    不良品数
     MOVE     RUI-F09             TO   MS01-FSURYOU.
*    不良品区分・区分名
     MOVE     98                  TO   JYO-F01.
     MOVE     RUI-F10             TO   MS01-FKBNCD
                                       JYO-F02.
     MOVE     SPACE               TO   MS01-FKBNNM.
     IF       RUI-F10  NOT = SPACE
              PERFORM  JYOKEN1-READ-SEC
              IF       JYOKEN1-INV-FLG = SPACE
                       MOVE  JYO-F03       TO   MS01-FKBNNM
              ELSE
                       MOVE  ALL NC"？"    TO   MS01-FKBNNM
              END-IF
     END-IF.
*    自社商品情報
     MOVE     "("                 TO   MS01-SYOCD(1:1).
     MOVE     RUI-F06             TO   MS01-SYOCD(2:8).
     MOVE     " "                 TO   MS01-SYOCD(10:1).
     MOVE     RUI-F071            TO   MS01-SYOCD(11:5).
     MOVE     "-"                 TO   MS01-SYOCD(16:1).
     MOVE     RUI-F072            TO   MS01-SYOCD(17:2).
     MOVE     "-"                 TO   MS01-SYOCD(19:1).
     MOVE     RUI-F073            TO   MS01-SYOCD(20:1).
     MOVE     ")"                 TO   MS01-SYOCD(21:1).
*    発注伝票情報
     MOVE     RUI-F11             TO   MS01-DENNO.
     MOVE     RUI-F12             TO   MS01-GYONO.
*    不良品検品担当者
     MOVE     PARA-IN-BUMON       TO   TAN-F01.
     MOVE     RUI-F04             TO   TAN-F02
                                       MS01-TANCD.
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG = SPACE
              MOVE  TAN-F03       TO   MS01-TANNM
     ELSE
              MOVE  ALL NC"？"    TO   MS01-TANNM
     END-IF.
*
*    明細印刷
     IF     ( BRK-RUI-F02-FLG     =   "STR" ) OR
            ( BRK-RUI-F02-FLG     =   "   " )
              WRITE   PRT-REC     FROM MS01 AFTER  1
              ADD     1           TO   LINE-CNT
              MOVE    "   "       TO   BRK-RUI-F02-FLG
              MOVE    RUI-F02     TO   BRK-RUI-F02
     ELSE
        IF    BRK-RUI-F02-FLG     =   "BRK"
              MOVE    SPACE       TO   PRT-REC
              WRITE   PRT-REC               AFTER  1
              WRITE   PRT-REC     FROM MS01 AFTER  1
              ADD     2           TO   LINE-CNT
              MOVE    "   "       TO   BRK-RUI-F02-FLG
              MOVE    RUI-F02     TO   BRK-RUI-F02
        END-IF
     END-IF.
     ADD      1                   TO   PRINT-OUT-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝倉庫不良品累積ファイル読込件数
     DISPLAY "SKFRRKL2 READ-CNT  = " SKFRRKL2-READ-CNT UPON CONS.
     DISPLAY "OUTPUT   PRINT-CNT = " PRINT-OUT-CNT UPON CONS.
*    DISPLAY "         ERR-CNT   = " SKFRRKL2-ERR-CNT UPON CONS.
     DISPLAY "         PAGE-CNT  = " PAGE-CNT UPON CONS.
*
     CLOSE     SKFRRKL2  ZSOKMS1  ZSHIMS1  JYOKEN1
               MEIMS1    TANMS1   PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ索引
****************************************************************
 ZSOKMS1-READ-SEC           SECTION.
     MOVE "ZSOKMS1-READ-SEC"      TO   S-NAME.
*
     READ       ZSOKMS1
         INVALID       MOVE "INV" TO   ZSOKMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
****************************************************************
*    仕入先マスタ索引
****************************************************************
 ZSHIMS1-READ-SEC           SECTION.
     MOVE "ZSHIMS1-READ-SEC"       TO   S-NAME.
*
     READ       ZSHIMS1
         INVALID       MOVE "INV" TO   ZSHIMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   ZSHIMS1-INV-FLG
     END-READ.
*
 ZSHIMS1-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル索引
****************************************************************
 JYOKEN1-READ-SEC           SECTION.
     MOVE "JYOKEN1-READ-SEC"       TO   S-NAME.
*
     READ       JYOKEN1
         INVALID       MOVE "INV" TO   JYOKEN1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ索引
****************************************************************
 MEIMS1-READ-SEC           SECTION.
     MOVE "MEIMS1-READ-SEC"       TO   S-NAME.
*
     READ       MEIMS1
         INVALID       MOVE "INV" TO   MEIMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
