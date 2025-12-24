****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　オンラインシステム                *
*    業務名　　　　　　　：　ナフコ出荷支援システム用　　　　　*
*    モジュール名　　　　：　ナフコ基本情報ファイル作成　　　　*
*    作成日／更新日　　　：　2015/06/16                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　バッチ_を受取り、ナフコ基本情報　
*                            Ｆを作成する。　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            NJH3752B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/06/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*変換伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-STATUS.
*基本情報ファイル
     SELECT   NFJOHOF   ASSIGN    TO        DA-01-VI-NFJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F02
                                            JOH-F03
                                            JOH-F04
                                            JOH-F05
                                            JOH-F06
                                            JOH-F07
                                            JOH-F08
                                            JOH-F09
                        FILE      STATUS    JOH-STATUS.
*作場マスタ
     SELECT   SAKUBAF   ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SKB-F01
                        FILE      STATUS    SKB-STATUS.
*作業番号マスタ
     SELECT   NFKANRF   ASSIGN    TO        DA-01-VI-NFKANRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KAR-F01
                                            KAR-F02
                                            KAR-F03
                        FILE      STATUS    KAR-STATUS.
*ナフコ商品名称マスタ
     SELECT   NFSHOMS   ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLA           LABEL RECORD   IS   STANDARD.
     COPY     NFSHIRED  OF        XFDLIB
              JOINING   DEN       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  NFJOHOF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JOH  AS   PREFIX.
******************************************************************
*    作場マスタ
******************************************************************
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SKB       PREFIX.
******************************************************************
*    作業番号ファイル
******************************************************************
 FD  NFKANRF            LABEL RECORD   IS   STANDARD.
     COPY     NFKANRF   OF        XFDLIB
              JOINING   KAR       PREFIX.
******************************************************************
*    ナフコ商品名称マスタ
******************************************************************
 FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-SAIBAN               PIC  9(11)     VALUE  ZERO.
 01  SAKUBAF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NFSHOMS-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  NFJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-JOH-F21              PIC  9(05)V9(01)  VALUE  ZERO.
 01  WK-JOH-F21-1            PIC  9(05)     VALUE  ZERO.
*売上伝票ファイル編集
 01  WK-DEN-REC.
     03  WK-DEN-F01          PIC  X(450)    VALUE  SPACE.
     03  WK-DEN-F02          PIC  X(128)    VALUE  SPACE.
     03  WK-DEN-F03          PIC  X(128)    VALUE  SPACE.
     03  WK-DEN-F04          PIC  X(128)    VALUE  SPACE.
     03  WK-DEN-F05          PIC  X(128)    VALUE  SPACE.
     03  FILLER              PIC  X(58)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SKB-STATUS        PIC  X(02).
     03  KAR-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH3752B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3752B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH3752B".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-SAIBAN            PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME
                                       PARA-TOKCD  PARA-SAIBAN.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF.
     MOVE      "NFJOHOL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SAKUBAF.
     MOVE      "SAKUBAL1"   TO   AB-FILE.
     MOVE      SKB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFKANRF.
     MOVE      "NFKANRL1"   TO   AB-FILE.
     MOVE      KAR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     SHTDENLA
                        SAKUBAF
                        NFSHOMS.
     OPEN     I-O       NFJOHOF   NFKANRF.
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
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*管理番号採番
     PERFORM   HJYOKEN-READ-SEC.
*売上伝票ファイルスタート
     MOVE      SPACE            TO   DEN-REC.
     INITIALIZE                      DEN-REC.
     MOVE      PARA-JDATE       TO   DEN-F46.
     MOVE      PARA-JTIME       TO   DEN-F47.
     MOVE      PARA-TOKCD       TO   DEN-F01.
     START  SHTDENLA  KEY  IS  >=    DEN-F46  DEN-F47  DEN-F01
                                     DEN-F48  DEN-F02  DEN-F04
                                     DEN-F051 DEN-F07  DEN-F112
                                     DEN-F03
            INVALID
            MOVE  "4050"        TO   PROGRAM-STATUS
            DISPLAY NC"処理対象がありません" UPON CONS
            STOP  RUN
     END-START.
*売上伝票ファイル読み込み
     PERFORM SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*存在チェック
     PERFORM  NFJOHOF-READ-SEC.
     IF  NFJOHOF-INV-FLG  =  "INV"
*        レコード転送処理
         PERFORM TENSO-SEC
     END-IF.
*売上伝票ファイル読み込み
     PERFORM SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*初期化
     MOVE     SPACE        TO        JOH-REC.
     INITIALIZE                      JOH-REC.
*管理番号
     MOVE        WK-SAIBAN TO        JOH-F01.
*取引先コード
     MOVE        DEN-F01   TO        JOH-F04.
*伝票NO.
     MOVE        DEN-F02   TO        JOH-F07.
*行_
     MOVE        DEN-F03   TO        JOH-F08.
*店コード
     MOVE        DEN-F07   TO        JOH-F06.
*出荷場所／伝発場所
     MOVE        DEN-F08   TO        JOH-F28.
*納品日
     MOVE        DEN-F112  TO        JOH-F09.
*自社商品コード
     MOVE        DEN-F1411 TO        JOH-F15.
*自社商品単品コード
     MOVE        DEN-F1412 TO        JOH-F16.
*数量
     MOVE        DEN-F15   TO        JOH-F19  JOH-F20.
*相手商品コード
     MOVE        DEN-F25   TO        JOH-F13.
*ＪＡＮＣＤ
     MOVE        DEN-A61   TO        JOH-F14.
*受信日付
     MOVE        DEN-F46   TO        JOH-F02.
*受信時刻
     MOVE        DEN-F47   TO        JOH-F03.
*振分倉庫コード
     MOVE        DEN-F48   TO        JOH-F05.
*作場マスタ索引
     MOVE   JOH-F05        TO        SKB-F01.
     PERFORM  SAKUBAF-READ-SEC.
     IF     SAKUBAF-INV-FLG = "INV"
            MOVE  "1"      TO        JOH-F12
     ELSE
            MOVE  SKB-F03  TO        JOH-F12
     END-IF.
*オンライン／手書区分
     MOVE        "1"       TO        JOH-F17.
*_番
     MOVE        DEN-F49   TO        JOH-F29.
*ナフコ商品名称マスタ読込
     MOVE        JOH-F13   TO        MEI-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF  NFSHOMS-INV-FLG = SPACE
         COMPUTE WK-JOH-F21   = JOH-F19 / MEI-F09 / MEI-F10
         COMPUTE WK-JOH-F21-1 = WK-JOH-F21 + 0.9
         MOVE    WK-JOH-F21-1 TO   JOH-F21  JOH-F22
         MOVE    MEI-F09      TO   JOH-F18
     ELSE
         MOVE    JOH-F19      TO   JOH-F21  JOH-F22
     END-IF.
*登録担当者部門
     MOVE        "2940"    TO        JOH-F30.
*登録担当者ＣＤ
     MOVE        "99"      TO        JOH-F31.
*データ転送
     MOVE    DEN-REC       TO        WK-DEN-REC.
     MOVE    WK-DEN-F02    TO        JOH-HE00.
     MOVE    WK-DEN-F03    TO        JOH-HF00.
     MOVE    WK-DEN-F04    TO        JOH-ME00.
     MOVE    WK-DEN-F05    TO        JOH-MF00.
*レコード追加
     WRITE   JOH-REC.
*
     ADD     1             TO        WRT-CNT.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF   WRT-CNT > ZERO
          PERFORM NFKANRF-WT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
*
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENLA  NFJOHOF  SAKUBAF  NFKANRF  NFSHOMS.
*
     MOVE      WK-SAIBAN TO        PARA-SAIBAN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC        SECTION.
*
     MOVE  "SHTDENF-READ-SEC"     TO    S-NAME.
*
     READ  SHTDENLA NEXT AT END
           MOVE   "END"           TO    END-FLG
           GO                     TO    SHTDENF-READ-EXIT
     END-READ.
*
     ADD   1                      TO    RD-CNT.
*バッチ_チェック
*****DISPLAY "DEN-F46 = "DEN-F46 UPON CONS.
*****DISPLAY "DEN-F47 = "DEN-F47 UPON CONS.
*****DISPLAY "DEN-F01 = "DEN-F01 UPON CONS.
*****DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
*****DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
*****DISPLAY "PARA-TOKCD = " PARA-TOKCD UPON CONS.
     IF    DEN-F46  =  PARA-JDATE
     AND   DEN-F47  =  PARA-JTIME
     AND   DEN-F01  =  PARA-TOKCD
           CONTINUE
     ELSE
           MOVE   "END"           TO    END-FLG
           GO                     TO    SHTDENF-READ-EXIT
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル読込　　　　　　　　　　　　　　　　*
****************************************************************
 HJYOKEN-READ-SEC        SECTION.
*
     MOVE   "HJYOKEN-READ-SEC"  TO   S-NAME.
*
     OPEN   I-O   HJYOKEN.
*
     MOVE   "99"           TO        JYO-F01.
     MOVE   "NAFUKO"       TO        JYO-F02.
     READ   HJYOKEN
       INVALID
            DISPLAY "## HJYOKEN INVALID KEY 99 NAFUKO" UPON CONS
            STOP  RUN
     END-READ.
*
     ADD    1              TO        JYO-F04.
*
     IF     JYO-F04  >  JYO-F06
            MOVE  JYO-F05  TO        JYO-F04
            MOVE  JYO-F04  TO        WK-SAIBAN
     ELSE
            MOVE  JYO-F04  TO        WK-SAIBAN
     END-IF.
*
     DISPLAY NC"＃管理番号" " = " WK-SAIBAN " " NC"＃"
             UPON CONS.
*
     REWRITE JYO-REC.
*
     CLOSE  HJYOKEN.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　作場マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 SAKUBAF-READ-SEC        SECTION.
*
     MOVE   "SAKUBAF-READ-SEC"  TO   S-NAME.
*
     READ   SAKUBAF
       INVALID
            MOVE   "INV"   TO        SAKUBAF-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        SAKUBAF-INV-FLG
     END-READ.
*
 SAKUBAF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ナフコ商品名称マスタ読込　　　　　　　　　　　　*
****************************************************************
 NFSHOMS-READ-SEC        SECTION.
*
     MOVE   "NFSHOMS-READ-SEC"  TO   S-NAME.
*
     READ   NFSHOMS
       INVALID
            MOVE   "INV"   TO        NFSHOMS-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ナフコ管理番号ファイル作成　　　　　　　　　　　*
****************************************************************
 NFKANRF-WT-SEC          SECTION.
*
     MOVE   "NFKANRF-WT-SEC"    TO   S-NAME.
*
     MOVE    SPACE              TO   KAR-REC.
     INITIALIZE                      KAR-REC.
*
     MOVE    "1"                TO   KAR-F01.
     MOVE    WK-SAIBAN          TO   KAR-F02.
     MOVE    PARA-JDATE         TO   KAR-F031.
     MOVE    PARA-JTIME         TO   KAR-F032.
     MOVE    PARA-TOKCD         TO   KAR-F033.
     MOVE    "99"               TO   KAR-F04.
     MOVE    SYS-DATEW          TO   KAR-F98.
     MOVE    WK-TIME(1:6)       TO   KAR-F99.
*
     WRITE   KAR-REC.
*
 NFKANRF-WT-EXIT.
     EXIT.
****************************************************************
*                ナフコ新ＥＤＩ　基本情報Ｆ読み込み            *
****************************************************************
 NFJOHOF-READ-SEC          SECTION.
*
     MOVE      "NFJOHOF-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F46            TO    JOH-F02.
     MOVE      DEN-F47            TO    JOH-F03.
     MOVE      DEN-F01            TO    JOH-F04.
     MOVE      DEN-F48            TO    JOH-F05.
     MOVE      DEN-F07            TO    JOH-F06.
     MOVE      DEN-F02            TO    JOH-F07.
     MOVE      DEN-F03            TO    JOH-F08.
     MOVE      DEN-F112           TO    JOH-F09.
*
     READ      NFJOHOF   INVALID
               MOVE      "INV"    TO    NFJOHOF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    NFJOHOF-INV-FLG
     END-READ.
*
 NFJOHOF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
