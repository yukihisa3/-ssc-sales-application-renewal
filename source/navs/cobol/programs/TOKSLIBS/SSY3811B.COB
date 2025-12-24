****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　オンラインシステム                *
*    業務名　　　　　　　：　ナフコ出荷支援システム　　　　　　*
*    モジュール名　　　　：　作場（オンライン）振分処理　　　　*
*    作成日／更新日　　　：　2015/04/27                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　該当バッチ_のデータに対し、
*                            各店舗への出荷元である作場コードを
*                            取得・更新する。
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY3811B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/04/27.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLI  ASSIGN    TO        DA-01-VI-SHTDENLI
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F02
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*基本情報ファイル
     SELECT   NFJOHOL2  ASSIGN    TO        DA-01-VI-NFJOHOL2
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
                                            WITH DUPLICATES
                        FILE      STATUS    JOH-STATUS.
*ナフコ店舗マスタ
     SELECT   NFTENMS1  ASSIGN    TO        DA-01-VI-NFTENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F01 TEN-F02
                        FILE      STATUS    TEN-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F02
                        FILE      STATUS    SHO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLI           LABEL RECORD   IS   STANDARD.
     COPY     SHTDENLI  OF        XFDLIB
              JOINING   DEN       PREFIX.
*
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  NFJOHOL2
                        LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOL2  OF        XFDLIB
              JOINING   JOH  AS   PREFIX.
******************************************************************
*    ナフコ店舗マスタ
******************************************************************
 FD  NFTENMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  DEN-UPDT-CNT            PIC  9(08)     VALUE  ZERO.
 01  JOH-UPDT-CNT            PIC  9(08)     VALUE  ZERO.
 01  WK-SAKUBA               PIC  X(02)     VALUE  SPACE.
*01  WK-SAIBAN               PIC  9(11)     VALUE  ZERO.
 01  NFTENMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFSHOMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  SHTDENLI-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFJOHOL2-INV-FLG        PIC  X(03)     VALUE  SPACE.
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
     03  TEN-STATUS        PIC  X(02).
     03  KAR-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3811B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3811B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3811B".
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
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " SHTDENLI".
         05  FILLER         PIC   X(09)  VALUE "  UPDATE=".
         05  OUT-CNT1       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT2.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " NFJOHOL2".
         05  FILLER         PIC   X(09)  VALUE "  UPDATE=".
         05  OUT-CNT2       PIC   9(06).
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
*01  PARA-SAIBAN            PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE  PARA-JTIME
                                       PARA-TOKCD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLI.
     MOVE      "SHTDENLI"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOL2.
     MOVE      "NFJOHOL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFTENMS1.
     MOVE      "NFTENMS1"   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
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
     OPEN     I-O       SHTDENLI.
     OPEN     I-O       NFJOHOL2.
     OPEN     INPUT     NFTENMS1  NFSHOMS1.
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
*売上伝票ファイルスタート
     MOVE      SPACE            TO   DEN-REC.
     INITIALIZE                      DEN-REC.
     MOVE      PARA-JDATE       TO   DEN-F46.
     MOVE      PARA-JTIME       TO   DEN-F47.
     MOVE      PARA-TOKCD       TO   DEN-F01.
     START  SHTDENLI  KEY  IS  >=    DEN-F46  DEN-F47  DEN-F01
                                     DEN-F02  DEN-F07  DEN-F112
                                     DEN-F03
            INVALID
             MOVE  "4010"        TO   PROGRAM-STATUS
             DISPLAY NC"処理対象がありません" UPON CONS
             STOP  RUN
     END-START.
*売上伝票ファイル読み込み
     PERFORM SHTDENLI-READ-SEC.
     IF      END-FLG   =  "END"
             DISPLAY NC"処理対象がありません" UPON CONS
             STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*基本情報ファイル（更新先）取得
     MOVE     DEN-F46    TO   JOH-F02
     MOVE     DEN-F47    TO   JOH-F03
     MOVE     DEN-F01    TO   JOH-F04
     MOVE     DEN-F48    TO   JOH-F05
     MOVE     DEN-F07    TO   JOH-F06
     MOVE     DEN-F02    TO   JOH-F07
     MOVE     DEN-F03    TO   JOH-F08
     MOVE     DEN-F112   TO   JOH-F09
     PERFORM  NFJOHOL2-READ-SEC.
     IF  NFJOHOL2-INV-FLG  =  "INV"
         DISPLAY NC"ナフコ基本情報ファイルなし！" UPON CONS
         DISPLAY NC"受信日　　＝" DEN-F46  UPON CONS
         DISPLAY NC"受信時刻　＝" DEN-F47  UPON CONS
         DISPLAY NC"受信取引先＝" DEN-F01  UPON CONS
         DISPLAY NC"倉庫ＣＤ　＝" DEN-F48  UPON CONS
         DISPLAY NC"店舗ＣＤ　＝" DEN-F07  UPON CONS
         DISPLAY NC"伝票番号　＝" DEN-F02  UPON CONS
         DISPLAY NC"行番号　　＝" DEN-F03  UPON CONS
         DISPLAY NC"納品日　　＝" DEN-F112 UPON CONS
         MOVE    4010    TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*
*ナフコ店舗マスタ－所属エリアＣＤ取得
     MOVE     DEN-F01    TO   TEN-F01
     MOVE     DEN-F07    TO   TEN-F02
     PERFORM  NFTENMS1-READ-SEC.
     IF  NFTENMS1-INV-FLG  =  "INV"
         DISPLAY NC"ナフコ店舗マスタなし！" UPON CONS
         DISPLAY NC"取引先ＣＤ＝" DEN-F01   UPON CONS
         DISPLAY NC"店舗ＣＤ　＝" DEN-F07   UPON CONS
         MOVE    4010    TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*
*ナフコ商品マスタ－出荷可能エリア取得
     MOVE     JOH-F13    TO   SHO-F02
     PERFORM  NFSHOMS1-READ-SEC.
     IF  NFSHOMS1-INV-FLG  =  "INV"
         DISPLAY NC"ナフコ商品マスタなし！" UPON CONS
         DISPLAY NC"商品ＣＤ　＝" JOH-F13   UPON CONS
         MOVE    4010    TO   PROGRAM-STATUS
         STOP    RUN
     END-IF.
*
*作場コード特定
     PERFORM SAKUBA-GET-SEC.
*
*ナフコ基本情報ファイル-作場（倉庫ＣＤ）更新
     MOVE    WK-SAKUBA  TO   JOH-F05
     PERFORM NFJOHOL2-UPDT-SEC.
*
*売上伝票ファイル-作場（振分倉庫ＣＤ）更新
     MOVE    WK-SAKUBA  TO   DEN-F48
     PERFORM SHTDENLI-UPDT-SEC.
*
*売上伝票ファイル読み込み
     PERFORM SHTDENLI-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込
****************************************************************
 SHTDENLI-READ-SEC        SECTION.
*
     MOVE  "SHTDENLI-READ-SEC"     TO    S-NAME.
*
     READ  SHTDENLI NEXT AT END
           MOVE   "END"           TO    END-FLG
           GO                     TO    SHTDENLI-READ-EXIT
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
           GO                     TO    SHTDENLI-READ-EXIT
     END-IF.
*
 SHTDENLI-READ-EXIT.
     EXIT.
****************************************************************
*                ナフコ基本情報Ｆ読み込み
****************************************************************
 NFJOHOL2-READ-SEC          SECTION.
*
     MOVE      "NFJOHOL2-READ-SEC" TO    S-NAME.
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
     READ      NFJOHOL2   INVALID
               MOVE      "INV"    TO    NFJOHOL2-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    NFJOHOL2-INV-FLG
     END-READ.
*
 NFJOHOL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ナフコ店舗マスタ読込　　　　　　　　　　　　　　*
****************************************************************
 NFTENMS1-READ-SEC        SECTION.
*
     MOVE   "NFTENMS1-READ-SEC"  TO   S-NAME.
*
     READ   NFTENMS1
       INVALID
            MOVE   "INV"   TO        NFTENMS1-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        NFTENMS1-INV-FLG
     END-READ.
*
 NFTENMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ナフコ商品マスタ読込　　　　　　　　　　　　*
****************************************************************
 NFSHOMS1-READ-SEC        SECTION.
*
     MOVE   "NFSHOMS1-READ-SEC"  TO   S-NAME.
*
     READ   NFSHOMS1
       INVALID
            MOVE   "INV"   TO        NFSHOMS1-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        NFSHOMS1-INV-FLG
     END-READ.
*
 NFSHOMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　作場コード特定                                  *
****************************************************************
 SAKUBA-GET-SEC          SECTION.
*
     MOVE   "SAKUBA-GET-SEC"    TO   S-NAME.
*
     MOVE   "  "                TO   WK-SAKUBA.
     EVALUATE   TEN-F13
           WHEN   SHO-F24
                  MOVE   SHO-F25     TO   WK-SAKUBA
           WHEN   SHO-F26
                  MOVE   SHO-F27     TO   WK-SAKUBA
           WHEN   SHO-F28
                  MOVE   SHO-F29     TO   WK-SAKUBA
           WHEN   SHO-F30
                  MOVE   SHO-F31     TO   WK-SAKUBA
           WHEN   SHO-F32
                  MOVE   SHO-F33     TO   WK-SAKUBA
           WHEN   SHO-F34
                  MOVE   SHO-F35     TO   WK-SAKUBA
           WHEN   SHO-F36
                  MOVE   SHO-F37     TO   WK-SAKUBA
           WHEN   SHO-F38
                  MOVE   SHO-F39     TO   WK-SAKUBA
           WHEN   SHO-F40
                  MOVE   SHO-F41     TO   WK-SAKUBA
           WHEN   SHO-F42
                  MOVE   SHO-F43     TO   WK-SAKUBA
           WHEN   OTHER
                  MOVE   SHO-F23     TO   WK-SAKUBA
     END-EVALUATE.
*
 SAKUBA-GET-EXIT.
     EXIT.
****************************************************************
*          ナフコ基本情報ファイル-作場（倉庫ＣＤ）更新
****************************************************************
 NFJOHOL2-UPDT-SEC     SECTION.
*
     MOVE    "NFJOHOL2-UPDT-SEC"     TO        S-NAME.
*
*レコード更新
     REWRITE JOH-REC.
*
     ADD     1             TO        JOH-UPDT-CNT.
*
 NFJOHOL2-UPDT-EXIT.
     EXIT.
****************************************************************
*          売上伝票ファイル-作場（振分倉庫ＣＤ）更新
****************************************************************
 SHTDENLI-UPDT-SEC     SECTION.
*
     MOVE    "SHTDENLI-UPDT-SEC"     TO        S-NAME.
*
*レコード更新
     REWRITE DEN-REC.
*
     ADD     1             TO        DEN-UPDT-CNT.
*
 SHTDENLI-UPDT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT        TO      IN-CNT.
     MOVE      DEN-UPDT-CNT  TO      OUT-CNT1.
     MOVE      JOH-UPDT-CNT  TO      OUT-CNT2.
*
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT1  UPON CONS.
     DISPLAY   MSG-OUT2  UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENLI  NFJOHOL2  NFTENMS1  NFSHOMS1.
*
*    MOVE      WK-SAIBAN TO        PARA-SAIBAN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
