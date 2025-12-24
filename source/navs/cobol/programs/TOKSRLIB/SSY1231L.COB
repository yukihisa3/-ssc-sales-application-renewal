****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　                        *
*    サブシステム　　　　：　コーナン　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　一括ＰＤラベル発行一覧表　　　　  *
*    作成日／作成者　　　：　2021/01/28 INOUE                  *
*    処理概要　　　　　　：　受け取り条件に従い、　　　　　　  *
*                            リストを発行する。                *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SSY1231L.
*               流用:SSY1212L.TOKSRLIB
 AUTHOR.             NAV.
 DATE-WRITTEN.       2021/01/28.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*一括ＰＤラベルファイル
     SELECT   KNBULKL2       ASSIGN        TO  01-VI-KNBULKL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  BUL-F001
                                               BUL-F002
                                               BUL-F003
                                               BUL-F004
                                               BUL-FA01
                                               BUL-FA02
                                               BUL-FA03
                                               BUL-FA06
                                               BUL-FA04
                                               BUL-FA05
                                               BUL-FB06
                             FILE STATUS   IS  BUL-STA.

*店舗Ｍ
     SELECT   TENMS1         ASSIGN        TO  01-VI-TENMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TEN-F52  TEN-F011
                             FILE STATUS   IS  TEN-STA.

*倉庫Ｍ
     SELECT   ZSOKMS1        ASSIGN        TO  01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-STA.

*商品名称Ｍ
*    SELECT   MEIMS1         ASSIGN        TO  01-VI-MEIMS1
*                            ORGANIZATION  IS  INDEXED
*                            ACCESS MODE   IS  RANDOM
*                            RECORD KEY    IS  SHO-F011
*                                              SHO-F012
*                            FILE STATUS   IS  TEN-STA.
*
*取引先Ｍ
     SELECT   TOKMS2         ASSIGN        TO  01-VI-TOKMS2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TOK-F01
                             FILE STATUS   IS  TOK-STA.

*プリンタＦ
     SELECT   PRTF           ASSIGN      TO  GS-XU04LP
                             ORGANIZATION         IS   SEQUENTIAL
                             ACCESS MODE          IS   SEQUENTIAL
                             SYMBOLIC DESTINATION IS  "PRT"
                             PROCESSING MODE      IS   PRT-PRO
                             GROUP                IS   PRT-GRP
                             FORMAT               IS   PRT-FMT
                             SELECTED FUNCTION    IS   PRT-FNC
                             UNIT     CONTROL     IS   PRT-CTL
                             FILE STATUS          IS   PRT-STA
                             DESTINATION-1        IS   PRT-DES.
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*一括ＰＤラベルファイル
 FD  KNBULKL2.
     COPY     KNBULKL2 OF  XFDLIB
     JOINING  BUL      AS  PREFIX.
*店舗Ｍ
 FD  TENMS1.
     COPY     HTENMS   OF  XFDLIB
     JOINING  TEN      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
*FD  MEIMS1.
*    COPY     HMEIMS   OF  XFDLIB
*    JOINING  SHO      AS  PREFIX.
*取引先Ｍ
 FD  TOKMS2.
     COPY     HTOKMS   OF  XFDLIB
     JOINING  TOK      AS  PREFIX.
*プリンタＦ
 FD  PRTF.
     COPY     FSY1231L OF  XMDLIB
     JOINING  PRT      AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*プリンタＦ制御用
 01  PRT-CONTROL.
     03  PRT-PRO           PIC  X(02).
     03  PRT-GRP           PIC  X(08).
     03  PRT-FMT           PIC  X(08).
     03  PRT-DES           PIC  X(08).
     03  PRT-CTL           PIC  X(06).
     03  PRT-FNC           PIC  X(04).
*ステータス
 01  STA-AREA.
     03  BUL-STA             PIC  X(02).
     03  TEN-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  SHO-STA             PIC  X(02).
     03  TOK-STA             PIC  X(02).
     03  PRT-STA             PIC  X(02).
 01  WORK-AREA.
     03  WK-SYSDT            PIC  9(06)  VALUE  ZERO.
     03  WK-SYSDTW           PIC  9(08)  VALUE  ZERO.
     03  WK-SYSDTW-R         REDEFINES   WK-SYSDTW.
       05  WK-SYSDT-YY       PIC  9(04).
       05  WK-SYSDT-MM       PIC  9(02).
       05  WK-SYSDT-DD       PIC  9(02).
     03  FLG-TOK             PIC  9(01)  VALUE  ZERO.
     03  MAIN-FLG            PIC  9(02)  VALUE  ZERO.
     03  BUL-FLG             PIC  9(01)  VALUE  ZERO.
     03  PAGE-FLG            PIC  9(01)  VALUE  ZERO.
     03  INIT-FLG            PIC  9(01)  VALUE  1.
     03  IX                  PIC  9(02)  VALUE  1.
*
*    03  WK-BDATE            PIC  9(08)  VALUE  ZERO.
*    03  WK-BTIME            PIC  9(04)  VALUE  ZERO.
*    03  WK-BTORCD           PIC  9(08)  VALUE  ZERO.
     03  WK-SOKOCD           PIC  X(02)  VALUE  SPACE.
     03  WK-TENCD            PIC  9(05)  VALUE  ZERO.
     03  WK-TORCD            PIC  9(08)  VALUE  ZERO.
     03  WK-ROUTE            PIC  9(02)  VALUE  ZERO.
     03  WK-BUMON            PIC  9(02)  VALUE  ZERO.
     03  WK-HACYU            PIC  9(06)  VALUE  ZERO.
*    03  WK-HACYU-R          REDEFINES   WK-HACYU.
*      05  WK-HACYU-YY       PIC  9(02).
*      05  WK-HACYU-MM       PIC  9(02).
*      05  WK-HACYU-DD       PIC  9(02).
     03  WK-HACYU-R.
       05  WK-HACYU-YY       PIC  9(04).
       05  WK-HACYU-MM       PIC  9(02).
       05  WK-HACYU-DD       PIC  9(02).
     03  WK-NOUHIN           PIC  9(06)  VALUE  ZERO.
*    03  WK-NOUHIN-R         REDEFINES   WK-NOUHIN.
*      05  WK-NOUHIN-YY      PIC  9(02).
*      05  WK-NOUHIN-MM      PIC  9(02).
*      05  WK-NOUHIN-DD      PIC  9(02).
     03  WK-NOUHIN-R.
       05  WK-NOUHIN-YY      PIC  9(04).
       05  WK-NOUHIN-MM      PIC  9(02).
       05  WK-NOUHIN-DD      PIC  9(02).
     03  WK-TANA             PIC  X(06)  VALUE  SPACE.
*
     03  WK-TENKB            PIC  X(01)  VALUE  SPACE.
*    03  WK-BUMON            PIC  X(04)  VALUE  SPACE.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
*ﾊﾞｯﾁNO.
 01  WK-BATCH.
     03  WK-JDATE            PIC  9(08)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  WK-JTIME            PIC  9(04)  VALUE  ZERO.
     03  FILLER              PIC  X(01)  VALUE  "-".
     03  WK-TORI             PIC  9(08)  VALUE  ZERO.
*メッセージ情報
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE  " *** ".
         05  FILLER          PIC  X(07)  VALUE  " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SSY1231L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** SSY1231L".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** KNBULKL2 ".
         05  FILLER          PIC  X(10)  VALUE  "  INPUT = ".
         05  CNT-READ        PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
     03  MSG-PRINT.
         05  FILLER          PIC  X(13)  VALUE  "*** PRTF     ".
         05  FILLER          PIC  X(10)  VALUE  "  PAGE  = ".
         05  CNT-WRITE       PIC  9(04)  VALUE  ZERO.
         05  FILLER          PIC  X(04)  VALUE  " ***".
*
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
*=============================================================
 LINKAGE             SECTION.
*=============================================================
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
 01  PARA-IN-SCENTER           PIC   9(05).
 01  PARA-IN-ECENTER           PIC   9(05).
 01  PARA-IN-SROUTE            PIC   9(02).
 01  PARA-IN-EROUTE            PIC   9(02).
 01  PARA-IN-SBUMON            PIC   9(02).
 01  PARA-IN-EBUMON            PIC   9(02).
 01  PARA-IN-HDATE             PIC   9(08).
 01  PARA-IN-NDATE             PIC   9(08).
 01  PARA-IN-STANA             PIC   X(06).
 01  PARA-IN-ETANA             PIC   X(06).
******************************************************************
 PROCEDURE               DIVISION
                         USING    PARA-IN-JDATE
                                  PARA-IN-JTIME
                                  PARA-IN-TORICD
                                  PARA-IN-SOKO
                                  PARA-IN-SCENTER
                                  PARA-IN-ECENTER
                                  PARA-IN-SROUTE
                                  PARA-IN-EROUTE
                                  PARA-IN-SBUMON
                                  PARA-IN-EBUMON
                                  PARA-IN-HDATE
                                  PARA-IN-NDATE
                                  PARA-IN-STANA
                                  PARA-IN-ETANA.
******************************************************************
 DECLARATIVES.
*プリントＦ
 PRT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       PRTF.
     MOVE    "PRTF"        TO    ERR-FL-ID.
     MOVE     PRT-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*一括ラベルファイル
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       KNBULKL2.
     MOVE    "KNBULKL2"    TO    ERR-FL-ID.
     MOVE     BUL-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*店舗Ｍ
 TEN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TENMS1.
     MOVE    "TENMS1"      TO    ERR-FL-ID.
     MOVE     TEN-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*倉庫Ｍ
 SOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       ZSOKMS1.
     MOVE    "ZSOKMS1"     TO    ERR-FL-ID.
     MOVE     SOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*商品名称Ｍ
*BUL-ERR-SEC        SECTION.
*    USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
*    MOVE    "MEIMS1"      TO    ERR-FL-ID.
*    MOVE     SHO-STA      TO    ERR-STCD.
*    DISPLAY  MSG-ABEND1   UPON  CONS.
*    DISPLAY  MSG-ABEND2   UPON  CONS.
*    DISPLAY  SEC-NAME     UPON  CONS.
*    STOP     RUN.
*取引先Ｍ
 TOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TOKMS2.
     MOVE    "TOKMS2"      TO    ERR-FL-ID.
     MOVE     TOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SSY1231L   START  **"   UPON  CONS.
*
     MOVE     "CONTROL-SEC"       TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SSY1231L   END    **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     MOVE    "INIT-SEC"   TO      S-NAME.
*ファイル ＯＰＥＮ
     OPEN     INPUT       KNBULKL2.
     OPEN     INPUT       TENMS1.
     OPEN     INPUT       ZSOKMS1.
*    OPEN     INPUT       MEIMS1.
     OPEN     INPUT       TOKMS2.
     OPEN     OUTPUT      PRTF.
*システム日付取得
     ACCEPT   WK-SYSDT    FROM  DATE.
     MOVE    "3"          TO         LINK-IN-KBN.
     MOVE     WK-SYSDT    TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-SYSDTW
     ELSE
         MOVE    ZERO           TO   WK-SYSDTW
     END-IF.
*ＭＡＸ行設定
     MOVE     62          TO    MAX-LINE.
     MOVE     SPACE       TO    PRT-CONTROL.
*一括ラベルファイルＳＴＡＲＴ
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*    DISPLAY  "PARA-IN-SCENTER=" PARA-IN-SCENTER UPON CONS.
*    DISPLAY  "PARA-IN-ECENTER=" PARA-IN-ECENTER UPON CONS.
*    DISPLAY  "PARA-IN-SROUTE =" PARA-IN-SROUTE  UPON CONS.
*    DISPLAY  "PARA-IN-EROUTE =" PARA-IN-EROUTE  UPON CONS.
*    DISPLAY  "PARA-IN-SBUMON =" PARA-IN-SBUMON  UPON CONS.
*    DISPLAY  "PARA-IN-EBUMON =" PARA-IN-EBUMON  UPON CONS.
*    DISPLAY  "PARA-IN-HDATE  =" PARA-IN-HDATE   UPON CONS.
*    DISPLAY  "PARA-IN-NDATE  =" PARA-IN-NDATE   UPON CONS.
*    DISPLAY  "PARA-IN-STANA  =" PARA-IN-STANA   UPON CONS.
*    DISPLAY  "PARA-IN-ETANA  =" PARA-IN-ETANA   UPON CONS.
*T
     MOVE      SPACE           TO   BUL-REC.
     INITIALIZE                     BUL-REC.
     MOVE      PARA-IN-JDATE   TO   BUL-F001.
     MOVE      PARA-IN-JTIME   TO   BUL-F002.
     MOVE      PARA-IN-TORICD  TO   BUL-F003.
     MOVE      PARA-IN-SOKO    TO   BUL-F004.
     MOVE      PARA-IN-SCENTER TO   BUL-FA01.
     MOVE      PARA-IN-TORICD  TO   BUL-FA02.
     MOVE      PARA-IN-SROUTE  TO   BUL-FA03.
     MOVE      PARA-IN-SBUMON  TO   BUL-FA06.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   BUL-FA04.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   BUL-FA05.
     MOVE      ZERO            TO   BUL-FB06.
*    MOVE      PARA-IN-STANA   TO   BUL-FC01.
     START     KNBULKL2   KEY  >=   BUL-F001  BUL-F002
                                    BUL-F003  BUL-F004
                                    BUL-FA01  BUL-FA02
                                    BUL-FA03  BUL-FA06
                                    BUL-FA04  BUL-FA05
                                    BUL-FB06
         INVALID   KEY
              MOVE    99     TO   MAIN-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*一括ラベルファイルＲＥＡＤ
     PERFORM  BUL-RD-SEC.
     IF  BUL-FLG   =  9
         MOVE  99         TO    MAIN-FLG
         GO               TO    INIT-EXIT
     END-IF.
     MOVE     BUL-F004    TO   WK-SOKOCD.
     MOVE     BUL-FA01    TO   WK-TENCD.
     MOVE     BUL-FA02    TO   WK-TORCD.
     MOVE     BUL-FA03    TO   WK-ROUTE.
     MOVE     BUL-FA06    TO   WK-BUMON.
     MOVE     BUL-FA04    TO   WK-HACYU.
     MOVE     BUL-FA05    TO   WK-NOUHIN.
*
     PERFORM   HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     MOVE    "MAIN-SEC"       TO   S-NAME.
 MAIN-00.
*倉庫ＣＤ ブレイク判定
     IF  BUL-F004 NOT  =  WK-SOKOCD
*        MOVE     BUL-F004    TO   WK-SOKOCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-01.
*センターＣＤブレイク判定
     IF  BUL-FA01     NOT  =  WK-TENCD
*        MOVE     BUL-FA01    TO   WK-TENCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-02.
*取引先ＣＤブレイク判定
     IF  BUL-FA02     NOT  =  WK-TORCD
*        MOVE     BUL-FA02    TO   WK-TORCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-03.
*ルートＣＤブレイク判定
     IF  BUL-FA03     NOT  =  WK-ROUTE
*        MOVE     BUL-FA03    TO   WK-ROUTE
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-04.
*部門ブレイク判定
     IF  BUL-FA06    NOT  =  WK-BUMON
*        MOVE     BUL-FA06    TO   WK-BUMON
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-05.
*発注日ブレイク判定
     IF  BUL-FA04 NOT  =  WK-HACYU
*        MOVE     BUL-FA04    TO   WK-HACYU
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-06.
*納品日ブレイク判定
     IF  BUL-FA05 NOT  =  WK-NOUHIN
*        MOVE     BUL-FA05    TO   WK-NOUHIN
         MOVE     1           TO   PAGE-FLG
     END-IF.
 MAIN-07.
*_番（上１桁）ブレイク判定
*    IF  BUL-FC01(1:1)    NOT   =   WK-TANA(1:1)
*        MOVE     BUL-FC01     TO   WK-TANA
*        MOVE     1            TO   PAGE-FLG
*        GO                    TO   MAIN-091
*    END-IF.
*
     IF  PAGE-FLG  =  1
*        IF  INIT-FLG  =  1
*            PERFORM  BODY-WT-SEC
*            MOVE     SPACE                  TO  PRT-BODYX
*            MOVE     1                      TO  IX
*            MOVE     0                      TO  INIT-FLG
*            GO                              TO  MAIN-08
*        END-IF
         GO                    TO   MAIN-091
     END-IF.
*
 MAIN-08.
*行編集
     EVALUATE  IX
       WHEN     1   MOVE   BUL-FB06  TO  PRT-NO0001
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     2   MOVE   BUL-FB06  TO  PRT-NO0002
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     3   MOVE   BUL-FB06  TO  PRT-NO0003
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     4   MOVE   BUL-FB06  TO  PRT-NO0004
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     5   MOVE   BUL-FB06  TO  PRT-NO0005
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     6   MOVE   BUL-FB06  TO  PRT-NO0006
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     7   MOVE   BUL-FB06  TO  PRT-NO0007
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     8   MOVE   BUL-FB06  TO  PRT-NO0008
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN     9   MOVE   BUL-FB06  TO  PRT-NO0009
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN    10   MOVE   BUL-FB06  TO  PRT-NO0010
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN    11   MOVE   BUL-FB06  TO  PRT-NO0011
                    ADD    1         TO  IX
                    GO               TO  MAIN-10
       WHEN    12   MOVE   BUL-FB06  TO  PRT-NO0012
*印刷
**                  MOVE  "FSY1231L"             TO  PRT-FMT
**                  MOVE  "A001"                 TO  PRT-CTL
**                  MOVE  "PW"                   TO  PRT-PRO
**                  MOVE  "BODY"                 TO  PRT-GRP
**                  WRITE  PRT-FSY1231L
**                  MOVE   SPACE                 TO  PRT-BODYX
**                  ADD    2                     TO  LINE-CNT
**                  MOVE   1         TO  IX
**                  GO               TO  MAIN-10
                    GO               TO  MAIN-09
*      WHEN    13   GO               TO  MAIN-09
     END-EVALUATE.
*
 MAIN-09.
*明細出力
     PERFORM  BODY-WT-SEC.
     MOVE     SPACE                  TO  PRT-BODYX.
     MOVE     1                      TO  IX.
*    MOVE     0                      TO  INIT-FLG.
     GO                              TO  MAIN-10.
*
 MAIN-091.
*明細出力
*    PERFORM  BODY-WT-SEC.
*印刷
     MOVE  "FSY1231L"             TO  PRT-FMT.
     MOVE  "A001"                 TO  PRT-CTL.
     MOVE  "PW"                   TO  PRT-PRO.
     MOVE  "BODY"                 TO  PRT-GRP.
     WRITE  PRT-FSY1231L.
     MOVE   SPACE                 TO  PRT-BODYX.
*    ADD    2                     TO  LINE-CNT.
     ADD    1                     TO  LINE-CNT.
     PERFORM  HEAD-WT-SEC.
     MOVE     ZERO         TO  PAGE-FLG.
     MOVE     BUL-F004               TO  WK-SOKOCD.
     MOVE     BUL-FA01               TO  WK-TENCD.
     MOVE     BUL-FA02               TO  WK-TORCD.
     MOVE     BUL-FA03               TO  WK-ROUTE.
     MOVE     BUL-FA06               TO  WK-BUMON.
     MOVE     BUL-FA04               TO  WK-HACYU.
     MOVE     BUL-FA05               TO  WK-NOUHIN.
     MOVE     SPACE                  TO  PRT-BODYX.
     MOVE     1                      TO  IX.
     MOVE     BUL-FB06               TO  PRT-NO0001.
     ADD      1                      TO  IX.
*
 MAIN-10.
*一括ラベルファイルREAD
     PERFORM  BUL-RD-SEC.
     IF  BUL-FLG   =  9
         MOVE  99                    TO  MAIN-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*=============================================================
*                ＨＥＡＤ部　印刷処理
*=============================================================
 HEAD-WT-SEC         SECTION.
     MOVE    "HEAD-WT-SEC"       TO  S-NAME.
     MOVE    SPACE               TO  PRT-HEADX.
*日付
     MOVE    WK-SYSDT-YY         TO  PRT-SYSYY.
     MOVE    WK-SYSDT-MM         TO  PRT-SYSMM.
     MOVE    WK-SYSDT-DD         TO  PRT-SYSDD.
*頁
     ADD   1                     TO  PAGE-CNT.
     MOVE  PAGE-CNT              TO  PRT-PAGE.
*ﾊﾞｯﾁNO.
     MOVE    BUL-F001            TO  WK-JDATE.
     MOVE    BUL-F002            TO  WK-JTIME.
     MOVE    BUL-F003            TO  WK-TORI.
     MOVE    WK-BATCH            TO  PRT-BATCH.
*取引先名
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    BUL-F003      TO    TOK-F01
*        MOVE    WK-TORCD      TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F02   TO        PRT-TORINM
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F02   TO        PRT-TORINM
     END-IF.
*ルート
     MOVE    BUL-FA03            TO  PRT-ROUTE.
*    MOVE    WK-ROUTE            TO  PRT-ROUTE.
*発注日
     MOVE    "3"          TO         LINK-IN-KBN.
*    MOVE     WK-HACYU    TO         LINK-IN-YMD6.
     MOVE     BUL-FA04    TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-HACYU-R
     ELSE
         MOVE    ZERO           TO   WK-HACYU-R
     END-IF.
     MOVE    WK-HACYU-YY         TO  PRT-HACYY.
     MOVE    WK-HACYU-MM         TO  PRT-HACMM.
     MOVE    WK-HACYU-DD         TO  PRT-HACDD.
*納品日
     MOVE    "3"          TO         LINK-IN-KBN.
*    MOVE     WK-NOUHIN   TO         LINK-IN-YMD6.
     MOVE     BUL-FA05    TO         LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING      LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   WK-NOUHIN-R
     ELSE
         MOVE    ZERO           TO   WK-NOUHIN-R
     END-IF.
     MOVE    WK-NOUHIN-YY        TO  PRT-NOUYY.
     MOVE    WK-NOUHIN-MM        TO  PRT-NOUMM.
     MOVE    WK-NOUHIN-DD        TO  PRT-NOUDD.
*倉庫名
     MOVE  WK-SOKOCD             TO  SOK-F01.
     READ  ZSOKMS1
           INVALID  KEY
              MOVE  SPACE        TO  PRT-SOKONM
           NOT INVALID  KEY
              MOVE  SOK-F02      TO  PRT-SOKONM
     END-READ.
*部門
     MOVE  BUL-FA06               TO  PRT-BUMON.
*    MOVE  WK-BUMON               TO  PRT-BUMON.
*センター
*    MOVE  BUL-F003               TO  TEN-F52.
     MOVE  WK-TORCD               TO  TEN-F52.
     MOVE  BUL-FA01               TO  PRT-TENCD  TEN-F011.
*    MOVE  WK-TENCD               TO  PRT-TENCD  TEN-F011.
     READ  TENMS1
           INVALID  KEY
                 MOVE  SPACE      TO  PRT-TENNM
           NOT INVALID  KEY
                 MOVE  TEN-F02    TO  PRT-TENNM
     END-READ.
*_番（１：１）の設定
*    MOVE  WK-TANA(1:1)         TO   PRT-TANA1.
*箱数記入欄
*    IF  ( PAGE-CNT   =  0 ) OR
*        ( PAGE-FLG   =  1 )
*          MOVE  SPACE          TO   PRT-HAKOSU
*    ELSE
*          MOVE  ALL NC"＊"     TO   PRT-HAKOSU
*    END-IF.
*印刷
     MOVE "FSY1231L"             TO  PRT-FMT.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "A000"                 TO  PRT-CTL.
     MOVE "HEAD"                 TO  PRT-GRP.
     WRITE PRT-FSY1231L.
*
     MOVE  9                     TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
     MOVE    "BODY-WT-SEC"     TO  S-NAME.
*BODY-WT-00.
*    IF  INIT-FLG   =  1
*        GO                    TO  BODY-WT-99
*    END-IF.
 BODY-WT-01.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
     OR  PAGE-FLG   =  1
         PERFORM  HEAD-WT-SEC
         MOVE     ZERO         TO  PAGE-FLG
     END-IF.
*明細部のクリア
*    MOVE  SPACE               TO  PRT-BODYX.
*_番
*    MOVE  BUL-FC01(2:3)       TO  PRT-TANA2.
*    MOVE  BUL-FC01(5:2)       TO  PRT-TANA3.
*商品名
*    MOVE  BUL-FC02            TO  SHO-F011.
*    MOVE  BUL-FC03            TO  SHO-F012.
*    READ  MEIMS1
*          INVALID
*             MOVE  ALL NC"＊" TO  PRT-SHONM
*          NOT INVALID  KEY
*             MOVE  SHO-F02    TO  PRT-SHONM
*             MOVE  SPACE      TO  PRT-KANANM
*    END-READ.
*
*数量
*    MOVE  BUL-FB08            TO  PRT-SURYO.
*訂正数量
*    IF    BUL-FB08  NOT  =  BUL-F10
*          MOVE  BUL-F10       TO  PRT-TSURYO
*    ELSE
*          MOVE  ZERO          TO  PRT-TSURYO
*    END-IF.
*売単価
*    MOVE  BUL-FB10            TO  PRT-URITAN.
*
*引当済み印
*    IF    BUL-FC12    =   "1"
*          MOVE   SPACE        TO  PRT-HOSHI
*    ELSE
*          MOVE   NC"★"       TO  PRT-HOSHI
*    END-IF.
*伝票_
*    MOVE  BUL-FA06            TO  PRT-DENNO.
*ＪＡＮＣＤ
*    MOVE  BUL-FB11            TO  PRT-JANCD.
*
 BODY-WT-99.
*印刷
     MOVE "FSY1231L"             TO  PRT-FMT.
     MOVE "A001"                 TO  PRT-CTL.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "BODY"                 TO  PRT-GRP.
     WRITE PRT-FSY1231L.
*    ADD   2                     TO  LINE-CNT.
     ADD   1                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                一括ラベルファイルリード処理
*=============================================================
 BUL-RD-SEC      SECTION.
     MOVE    "BUL-RD-SEC"  TO  S-NAME.
 BUL-RD-00.
*リード
     READ  KNBULKL2      NEXT
         AT   END
           MOVE   9        TO  BUL-FLG
           GO              TO  BUL-RD-EXIT
     END-READ.
     ADD   1               TO         CNT-READ.
*対象データチェック
 BUL-RD-01.
     IF ( PARA-IN-JDATE     =    BUL-F001 ) AND
        ( PARA-IN-JTIME     =    BUL-F002 ) AND
        ( PARA-IN-TORICD    =    BUL-F003 )
*       ( PARA-IN-TORICD    =    BUL-FA02 )
        CONTINUE
     ELSE
        MOVE   9         TO   BUL-FLG
        GO               TO   BUL-RD-EXIT
     END-IF.
*
 BUL-RD-02.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     BUL-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE   9         TO   BUL-FLG
                GO               TO   BUL-RD-EXIT
         END-IF
     END-IF.
*
 BUL-RD-03.
     IF  PARA-IN-SCENTER   IS     NUMERIC
         IF     BUL-FA01    >=      PARA-IN-SCENTER
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
 BUL-RD-04.
     IF  PARA-IN-ECENTER   IS     NUMERIC
         IF     BUL-FA01    <=  PARA-IN-ECENTER
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
        END-IF
     END-IF.
*
 BUL-RD-05.
     IF  PARA-IN-TORICD IS     NUMERIC
         IF     BUL-FA02     =  PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
        END-IF
     END-IF.
*
 MAIN006.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     BUL-FA03    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
 BUL-RD-07.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     BUL-FA03    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
 BUL-RD-08.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     BUL-FA06    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
 BUL-RD-09.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     BUL-FA06    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
 BUL-RD-10.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     BUL-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
 BUL-RD-11.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     BUL-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   BUL-RD-00
         END-IF
     END-IF.
*
*BUL-RD-12.
*    IF  PARA-IN-STANA  NOT =  SPACE
*        IF     BUL-FC01    >=  PARA-IN-STANA
*               CONTINUE
*        ELSE
*               GO          TO   BUL-RD-00
*        END-IF
*    END-IF.
*
*BUL-RD-13.
*    IF  PARA-IN-ETANA  NOT =  SPACE
*        IF     BUL-FC01    <=  PARA-IN-ETANA
*               CONTINUE
*        ELSE
*               GO          TO   BUL-RD-00
*        END-IF
*    END-IF.
*
*
 BUL-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
     MOVE      "END-SEC"     TO     S-NAME.
*ファイル ＣＬＯＳＥ
     IF         IX   NOT = 1
                PERFORM  BODY-WT-SEC
     END-IF.
     CLOSE      PRTF.
     CLOSE      KNBULKL2.
     CLOSE      TENMS1.
     CLOSE      ZSOKMS1.
*    CLOSE      MEIMS1.
     CLOSE      TOKMS2.
*ＭＳＧ出力
     DISPLAY    MSG-INPUT    UPON   CONS.
     MOVE       PAGE-CNT     TO     CNT-WRITE.
     DISPLAY    MSG-PRINT    UPON   CONS.
 END-EXIT.
     EXIT.
