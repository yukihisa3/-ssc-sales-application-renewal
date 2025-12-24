****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　                        *
*    サブシステム　　　　：　コーナン　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　ケースＰＤラベル発行一覧表　　　  *
*    作成日／作成者　　　：　2021/01/29 INOUE                  *
*    処理概要　　　　　　：　受け取り条件に従い、　　　　　　  *
*                            リストを発行する。                *
*    更新日／更新者　　　：　2021/05/18 INOUE
*　　　　　　　　　　　　　　KNCASEL2 KEY追加
*　                                                            *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SSY1232L.
*               流用:SSY1215L
 AUTHOR.             NAV.
 DATE-WRITTEN.       2021/01/29.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*ケースＰＤラベルファイル
     SELECT   KNCASEL2       ASSIGN        TO  01-VI-KNCASEL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  CAS-F001
                                               CAS-F002
                                               CAS-F003
                                               CAS-F004
                                               CAS-FA02
                                               CAS-FA07
                                               CAS-FA08
                                               CAS-FA03
                                               CAS-FA04
                                               CAS-FA05
                                               CAS-FC01
                                               CAS-FA06
                                               CAS-FB05
                                               CAS-FD05
*2021/05/18↓
                                               CAS-FD03
*2021/05/18↑
                             FILE STATUS   IS  CAS-STA.

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
     SELECT   MEIMS1         ASSIGN        TO  01-VI-MEIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHO-F011
                                               SHO-F012
                             FILE STATUS   IS  TEN-STA.

*取引先Ｍ
     SELECT   TOKMS2         ASSIGN        TO  01-VI-TOKMS2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  TOK-F01
                             FILE STATUS   IS  TOK-STA.

*売上伝票ファイル
     SELECT   SHTDENLA       ASSIGN        TO  DA-01-VI-SHTDENLA
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  DEN-F46   DEN-F47
                                               DEN-F01   DEN-F48
                                               DEN-F02   DEN-F04
                                               DEN-F051  DEN-F07
                                               DEN-F112  DEN-F03
                             FILE STATUS  IS   DEN-STA.
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
*ケースＰＤラベルファイル
 FD  KNCASEL2.
     COPY     KNCASEL2 OF  XFDLIB
     JOINING  CAS      AS  PREFIX.
*店舗Ｍ
 FD  TENMS1.
     COPY     HTENMS   OF  XFDLIB
     JOINING  TEN      AS  PREFIX.
*倉庫Ｍ
 FD  ZSOKMS1.
     COPY     ZSOKMS   OF  XFDLIB
     JOINING  SOK      AS  PREFIX.
*商品名称Ｍ
 FD  MEIMS1.
     COPY     HMEIMS   OF  XFDLIB
     JOINING  SHO      AS  PREFIX.
*取引先Ｍ
 FD  TOKMS2.
     COPY     HTOKMS   OF  XFDLIB
     JOINING  TOK      AS  PREFIX.
*
*売上伝票ファイル
 FD  SHTDENLA           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*プリンタＦ
 FD  PRTF.
     COPY     FSY1232L OF  XMDLIB
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
     03  CAS-STA             PIC  X(02).
     03  TEN-STA             PIC  X(02).
     03  SOK-STA             PIC  X(02).
     03  SHO-STA             PIC  X(02).
     03  TOK-STA             PIC  X(02).
     03  DEN-STA             PIC  X(02).
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
     03  CAS-FLG             PIC  9(01)  VALUE  ZERO.
     03  PAGE-FLG            PIC  9(01)  VALUE  ZERO.
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
     03  WK-TENKB            PIC  X(01)  VALUE  SPACE.
*    03  WK-BUMON            PIC  X(04)  VALUE  SPACE.
     03  LINE-CNT            PIC  9(02)  VALUE  ZERO.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  PAGE-CNT            PIC  9(04)  VALUE  ZERO.
     03  WK-SHONM            PIC  N(30).
     03  WK-SHONM-R  REDEFINES  WK-SHONM.
       05  WK-SHONM1         PIC  N(15).
       05  WK-SHONM2         PIC  N(15).
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
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
         05  FILLER          PIC  X(12)  VALUE  "### SSY1232L".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
     03  MSG-EXIT.
         05  FILLER          PIC  X(12)  VALUE  "*** SSY1232L".
         05  FILLER          PIC  X(11)  VALUE  "    END ***".
     03  MSG-INPUT.
         05  FILLER          PIC  X(13)  VALUE  "*** KNCASEL2 ".
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
 PROCEDURE              DIVISION USING
                                  PARA-IN-JDATE
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
*ケースＰＤラベルファイル
 OUT-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       KNCASEL2.
     MOVE    "KNCASEL2"    TO    ERR-FL-ID.
     MOVE     CAS-STA      TO    ERR-STCD.
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
 CAS-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       MEIMS1.
     MOVE    "MEIMS1"      TO    ERR-FL-ID.
     MOVE     SHO-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*取引先Ｍ
 TOK-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       TOKMS2.
     MOVE    "TOKMS2"      TO    ERR-FL-ID.
     MOVE     TOK-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*売上伝票ファイル
 DEN-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       SHTDENLA.
     MOVE    "SHTDENLA"    TO    ERR-FL-ID.
     MOVE     DEN-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     DISPLAY  SEC-NAME     UPON  CONS.
     STOP     RUN.
*
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SSY1232L   START  **"   UPON  CONS.
*
     MOVE     "CONTROL-SEC"       TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC    UNTIL  MAIN-FLG  =  99.
     PERFORM  END-SEC.
*
     DISPLAY  "**  SSY1232L   END    **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 INIT-SEC            SECTION.
     MOVE    "INIT-SEC"   TO      S-NAME.
*ファイル ＯＰＥＮ
 INIT-00.
     OPEN     INPUT       KNCASEL2.
     OPEN     INPUT       TENMS1.
     OPEN     INPUT       ZSOKMS1.
     OPEN     INPUT       MEIMS1.
     OPEN     INPUT       TOKMS2.
     OPEN     INPUT       SHTDENLA.
     OPEN     OUTPUT      PRTF.
*システム日付取得
 INIT-01.
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
 INIT-02.
     MOVE     62          TO    MAX-LINE.
     MOVE     SPACE       TO    PRT-CONTROL.
*ケースＰＤラベルファイルＳＴＡＲＴ
 INIT-03.
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
     MOVE      SPACE           TO   CAS-REC.
     INITIALIZE                     CAS-REC.
     MOVE      PARA-IN-JDATE   TO   CAS-F001.
     MOVE      PARA-IN-JTIME   TO   CAS-F002.
     MOVE      PARA-IN-TORICD  TO   CAS-F003.
     MOVE      PARA-IN-SOKO    TO   CAS-F004.
     MOVE      PARA-IN-SCENTER TO   CAS-FA02.
     MOVE      PARA-IN-TORICD  TO   CAS-FA07.
     MOVE      PARA-IN-SROUTE  TO   CAS-FA08.
     MOVE      PARA-IN-SBUMON  TO   CAS-FA03.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   CAS-FA04.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   CAS-FA05.
     MOVE      PARA-IN-STANA   TO   CAS-FC01.
     MOVE      ZERO            TO   CAS-FB06.
     START     KNCASEL2   KEY  >=   CAS-F001  CAS-F002
                                    CAS-F003  CAS-F004
                                    CAS-FA02  CAS-FA07
                                    CAS-FA08  CAS-FA03
                                    CAS-FA04  CAS-FA05
                                    CAS-FC01  CAS-FA06
                                    CAS-FB05  CAS-FD05
         INVALID   KEY
              MOVE    99     TO   MAIN-FLG
              GO             TO   INIT-EXIT
     END-START.
*
 INIT-04.
*
*ケースＰＤラベルファイルリード
     PERFORM  CAS-RD-SEC.
     IF  CAS-FLG   =  9
         MOVE  99         TO    MAIN-FLG
     END-IF.
*
 INIT-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
     MOVE    "MAIN-SEC"       TO   S-NAME.
 MAIN-01.
*倉庫ＣＤ ブレイク判定
     IF  CAS-F004 NOT  =  WK-SOKOCD
*T
*        DISPLAY "CAS-F004 =" CAS-F004
*        DISPLAY "WK-SOKOCD=" WK-SOKOCD
*T
         MOVE     CAS-F004    TO   WK-SOKOCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*センターＣＤブレイク判定
 MAIN-02.
     IF  CAS-FA02     NOT  =  WK-TENCD
*T
*        DISPLAY "CAS-FA02 =" CAS-FA02
*        DISPLAY "WK-TENCD =" WK-TENCD
*T
         MOVE     CAS-FA02    TO   WK-TENCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*取引先ＣＤブレイク判定
 MAIN-03.
     IF  CAS-FA07     NOT  =  WK-TORCD
*T
*        DISPLAY "CAS-FA07 =" CAS-FA07
*        DISPLAY "WK-TORCD =" WK-TORCD
*T
         MOVE     CAS-FA07    TO   WK-TORCD
         MOVE     1           TO   PAGE-FLG
     END-IF.
*ルートＣＤブレイク判定
 MAIN-04.
     IF  CAS-FA08     NOT  =  WK-ROUTE
*T
*        DISPLAY "CAS-FA08 =" CAS-FA08
*        DISPLAY "WK-ROUTE =" WK-ROUTE
*T
         MOVE     CAS-FA08    TO   WK-ROUTE
         MOVE     1           TO   PAGE-FLG
     END-IF.
*部門ブレイク判定
 MAIN-05.
     IF  CAS-FA03    NOT  =  WK-BUMON
*T
*        DISPLAY "CAS-FA03 =" CAS-FA03
*        DISPLAY "WK-BUMON =" WK-BUMON
*T
         MOVE     CAS-FA03    TO   WK-BUMON
         MOVE     1           TO   PAGE-FLG
     END-IF.
*発注日ブレイク判定
 MAIN-06.
     IF  CAS-FA04 NOT  =  WK-HACYU
*T
*        DISPLAY "CAS-FA04 =" CAS-FA04
*        DISPLAY "WK-HACYU =" WK-HACYU
*T
         MOVE     CAS-FA04    TO   WK-HACYU
         MOVE     1           TO   PAGE-FLG
     END-IF.
*納品日ブレイク判定
 MAIN-07.
     IF  CAS-FA05 NOT  =  WK-NOUHIN
*T
*        DISPLAY "CAS-FA05 =" CAS-FA05
*        DISPLAY "WK-NOUHIN=" WK-NOUHIN
*T
         MOVE     CAS-FA05    TO   WK-NOUHIN
         MOVE     1           TO   PAGE-FLG
     END-IF.
*_番（上１桁）ブレイク判定
 MAIN-08.
     IF  CAS-FC01(1:1)    NOT   =   WK-TANA(1:1)
*T
*        DISPLAY "CAS-FC01 =" CAS-FC01
*        DISPLAY "WK-TANA  =" WK-TANA
*T
         MOVE     CAS-FC01     TO   WK-TANA
         MOVE     1            TO   PAGE-FLG
     END-IF.
 MAIN-09.
*明細出力
     PERFORM  BODY-WT-SEC.
     IF  MAIN-FLG  =  99
         GO   TO   MAIN-EXIT
     END-IF.
 MAIN-10.
*ケースＰＤラベルファイルリード
     PERFORM  CAS-RD-SEC.
     IF  CAS-FLG   =  9
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
     MOVE    CAS-F001            TO  WK-JDATE.
     MOVE    CAS-F002            TO  WK-JTIME.
     MOVE    CAS-F003            TO  WK-TORI.
     MOVE    WK-BATCH            TO  PRT-BATCH.
*取引先名
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    CAS-F003      TO    TOK-F01
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
     MOVE    CAS-FA08            TO  PRT-ROUTE.
*発注日
     MOVE    "3"          TO         LINK-IN-KBN.
     MOVE     WK-HACYU    TO         LINK-IN-YMD6.
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
     MOVE     WK-NOUHIN   TO         LINK-IN-YMD6.
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
     MOVE  CAS-FA03               TO  PRT-BUMON.
*センター
     MOVE  CAS-F003               TO  TEN-F52.
     MOVE  CAS-FA02               TO  PRT-TENCD  TEN-F011.
     READ  TENMS1
           INVALID  KEY
                 MOVE  SPACE      TO  PRT-TENNM
           NOT INVALID  KEY
                 MOVE  TEN-F02    TO  PRT-TENNM
     END-READ.
*_番（１：１）の設定
     MOVE  WK-TANA(1:1)         TO   PRT-TANA1.
*箱数記入欄
*    IF  ( PAGE-CNT   =  0 ) OR
*        ( PAGE-FLG   =  1 )
*          MOVE  SPACE          TO   PRT-HAKOSU
*    ELSE
*          MOVE  ALL NC"＊"     TO   PRT-HAKOSU
*    END-IF.
*印刷
     MOVE "FSY1232L"             TO  PRT-FMT.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "A000"                 TO  PRT-CTL.
     MOVE "HEAD"                 TO  PRT-GRP.
     WRITE PRT-FSY1232L.
*
     MOVE  9                     TO  LINE-CNT.
 HEAD-WT-EXIT.
     EXIT.
*=============================================================
*                明細印刷処理
*=============================================================
 BODY-WT-SEC                SECTION.
     MOVE    "BODY-WT-SEC"     TO  S-NAME.
*改ページ
     IF  LINE-CNT  >=  MAX-LINE
     OR  PAGE-CNT   =  ZERO
     OR  PAGE-FLG   =  1
*T
*        DISPLAY "LINE-CNT=" LINE-CNT
*        DISPLAY "MAX-LINE=" MAX-LINE
*        DISPLAY "PAGE-CNT=" PAGE-CNT
*        DISPLAY "PAGE-FLG=" PAGE-FLG
*T
         PERFORM  HEAD-WT-SEC
         MOVE     ZERO         TO  PAGE-FLG
     END-IF.
*明細部のクリア
     MOVE  SPACE               TO  PRT-BODYX.
*_番
     MOVE  CAS-FC01(2:3)       TO  PRT-TANA2.
     MOVE  CAS-FC01(5:2)       TO  PRT-TANA3.
*商品名
     MOVE  CAS-FC02            TO  SHO-F011.
     MOVE  CAS-FC03            TO  SHO-F012.
     READ  MEIMS1
           INVALID
              MOVE  ALL NC"＊" TO  PRT-SHONM
*             MOVE  SPACE      TO  PRT-SHONM
*             MOVE  CAS-F081   TO  PRT-KANANM(1:15)
*             MOVE  CAS-F082   TO  PRT-KANANM(16:15)
           NOT INVALID  KEY
              MOVE  SHO-F02    TO  PRT-SHONM
*             MOVE  SPACE      TO  PRT-KANANM
     END-READ.
*
*荷番
     MOVE  CAS-FD03(15:5)      TO  PRT-NIBAN.
*数量
*    MOVE  CAS-FB08            TO  PRT-SURYO.
*ケース数
*    MOVE  CAS-FB07            TO  PRT-CASE.
*訂正数量
*    IF    CAS-FB08  NOT  =  CAS-F10
*          MOVE  CAS-F10       TO  PRT-TSURYO
*    ELSE
*          MOVE  ZERO          TO  PRT-TSURYO
*    END-IF.
*売単価
*    MOVE  CAS-FB10            TO  PRT-URITAN.
*
*引当済み印
*    IF    CAS-FC12    =   "1"
*          MOVE   SPACE        TO  PRT-HOSHI
*    ELSE
*          MOVE   NC"★"       TO  PRT-HOSHI
*    END-IF.
*売上伝票ファイル　検索
     MOVE     PARA-IN-JDATE(1:4)     TO  DEN-F461.
     MOVE     PARA-IN-JDATE(5:2)     TO  DEN-F462.
     MOVE     PARA-IN-JDATE(7:2)     TO  DEN-F463.
     MOVE     PARA-IN-JTIME(1:2)     TO  DEN-F471.
     MOVE     PARA-IN-JTIME(3:2)     TO  DEN-F472.
     MOVE     PARA-IN-TORICD         TO  DEN-F01.
     MOVE     CAS-F004               TO  DEN-F48.
     MOVE     CAS-FA06               TO  DEN-F02.
     MOVE     0                      TO  DEN-F04.
     MOVE     40                     TO  DEN-F051.
     MOVE     CAS-FA02               TO  DEN-F07.
     MOVE    "3"                     TO  LINK-IN-KBN.
     MOVE     CAS-FA05               TO  LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   DEN-F112
     ELSE
         MOVE    ZERO           TO   DEN-F112
     END-IF.
     MOVE     1                      TO  DEN-F03.
     READ     SHTDENLA
       INVALID
             DISPLAY NC"売上伝票ファイルなし！" UPON CONS
             DISPLAY NC"バッチ（日）＝" PARA-IN-JDATE UPON CONS
             DISPLAY NC"バッチ（時）＝" PARA-IN-JTIME UPON CONS
             DISPLAY NC"バッチ（取）＝" PARA-IN-TORICD UPON CONS
             DISPLAY NC"振分倉庫　　＝" CAS-F004       UPON CONS
             DISPLAY NC"伝票番号　　＝" CAS-FA06       UPON CONS
             DISPLAY NC"相殺区分　　＝" "0"            UPON CONS
             DISPLAY NC"伝区コード　＝" "40"           UPON CONS
             DISPLAY NC"店舗コード　＝" CAS-FA02       UPON CONS
             DISPLAY NC"納品日　　　＝" CAS-FA05       UPON CONS
             DISPLAY NC"行　　　　　＝" "1"            UPON CONS
             MOVE    4010            TO  PROGRAM-STATUS
             MOVE    99              TO  MAIN-FLG
             GO                      TO  BODY-WT-EXIT
     END-READ.
     IF    DEN-F27D    =   "1"
           MOVE   SPACE        TO  PRT-HOSHI
     ELSE
           MOVE   NC"★"       TO  PRT-HOSHI
     END-IF.
*
*
*伝票_
     MOVE  CAS-FA06            TO  PRT-DENNO.
*取引先商品コード
     MOVE  CAS-FB05            TO  PRT-JANCD.
*
*印刷
     MOVE "FSY1232L"             TO  PRT-FMT.
     MOVE "A001"                 TO  PRT-CTL.
     MOVE "PW"                   TO  PRT-PRO.
     MOVE "BODY"                 TO  PRT-GRP.
     WRITE PRT-FSY1232L.
     ADD   2                     TO  LINE-CNT.
 BODY-WT-EXIT.
     EXIT.
*=============================================================
*                ケースＰＤラベルファイルリード処理
*=============================================================
 CAS-RD-SEC      SECTION.
     MOVE    "CAS-RD-SEC"  TO  S-NAME.
 CAS-RD-00.
*リード
     READ  KNCASEL2      NEXT
         AT   END
           MOVE   9        TO  CAS-FLG
           GO              TO  CAS-RD-EXIT
     END-READ.
     ADD   1               TO         CNT-READ.
*対象データチェック
 CAS-RD-01.
     IF ( PARA-IN-JDATE     =    CAS-F001 ) AND
        ( PARA-IN-JTIME     =    CAS-F002 ) AND
        ( PARA-IN-TORICD    =    CAS-F003 )
        CONTINUE
     ELSE
        MOVE   9         TO   CAS-FLG
        GO               TO   CAS-RD-EXIT
     END-IF.
*
 CAS-RD-02.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     CAS-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE   9         TO   CAS-FLG
                GO               TO   CAS-RD-EXIT
         END-IF
     END-IF.
*
 CAS-RD-03.
     IF  PARA-IN-SCENTER   IS     NUMERIC
         IF     CAS-FA02    >=      PARA-IN-SCENTER
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-04.
     IF  PARA-IN-ECENTER   IS     NUMERIC
         IF     CAS-FA02    <=  PARA-IN-ECENTER
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
        END-IF
     END-IF.
*
 CAS-RD-05.
     IF  PARA-IN-TORICD IS     NUMERIC
         IF     CAS-FA07     =  PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
        END-IF
     END-IF.
*
 CAS-RD-06.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     CAS-FA08    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-07.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     CAS-FA08    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-08.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     CAS-FA03    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-09.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     CAS-FA03    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-10.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     CAS-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-11.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     CAS-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-12.
     IF  PARA-IN-STANA  NOT =  SPACE
         IF     CAS-FC01    >=  PARA-IN-STANA
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-13.
     IF  PARA-IN-ETANA  NOT =  SPACE
         IF     CAS-FC01    <=  PARA-IN-ETANA
                CONTINUE
         ELSE
                GO          TO   CAS-RD-00
         END-IF
     END-IF.
*
 CAS-RD-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
     MOVE      "END-SEC"     TO     S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE      PRTF.
     CLOSE      KNCASEL2.
     CLOSE      TENMS1.
     CLOSE      ZSOKMS1.
     CLOSE      MEIMS1.
     CLOSE      TOKMS2.
     CLOSE      SHTDENLA.
*ＭＳＧ出力
     DISPLAY    MSG-INPUT    UPON   CONS.
     MOVE       PAGE-CNT     TO     CNT-WRITE.
     DISPLAY    MSG-PRINT    UPON   CONS.
 END-EXIT.
     EXIT.
