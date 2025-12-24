****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　梱包数カウント　　　　　　　　　　*
*　　　　　　　　　　　　　　（カインズ店直）　　　　　　　　　*
*　　作成日／作成者　　　：　2019/06/25  NAV                   *
*　　処理概要　　　　　　：　作成対象データ中の梱包数をカウン　*
*　　　　　　　　　　　　　　ト。送信データ　出荷梱包数　　　  *
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE1042B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/25.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< カインズ出荷データ >>--*
     SELECT   CZSYKTNF  ASSIGN              DA-01-VI-CZSYKTN6
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SYK-F011
                                                 SYK-F012
                                                 SYK-F013
                                                 SYK-F02
                                                 SYK-F349
                                                 SYK-F348
                                                 SYK-F305
                                                 SYK-F309
                                                 SYK-F604
                                                 SYK-F605
                                                 SYK-F302
                                                 SYK-F402
                        FILE      STATUS    IS   SYK-ST.
*----<< 梱包数集計ファイル >>--*
     SELECT   CZKONGKF  ASSIGN              DA-01-VS-CZKONGKF
*                       ORGANIZATION        IS   INDEXED
*                       ACCESS    MODE      IS   RANDOM
*                       RECORD    KEY       IS   KON-F01
*                                                KON-F02
*                                                KON-F03
*                                                KON-F04
*                                                KON-F05
*                                                KON-F06
*                                                KON-F07
                        FILE      STATUS    IS   KON-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< カインズ出荷データ >>--*
 FD  CZSYKTNF    LABEL RECORD   IS   STANDARD.
     COPY        CZSYKTNF    OF      XFDLIB
                 JOINING     SYK     PREFIX.
*----<< 梱包数集計ファイル >>--*
 FD  CZKONGKF    LABEL RECORD   IS   STANDARD.
     COPY        CZKONGKF    OF      XFDLIB
                 JOINING     KON     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  TAISYO-CNT     PIC  9(07).
     03  KON-CNT        PIC  9(07).
     03  OUT-CNT        PIC  9(07).
 01  BRK-KEY.
     03  BRK-KEY-F011   PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F012   PIC  9(04)  VALUE  ZERO.
     03  BRK-KEY-F013   PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F02    PIC  X(02)  VALUE  SPACE.
     03  BRK-KEY-F349   PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F348   PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F305   PIC  X(13)  VALUE  SPACE.
     03  BRK-KEY-F605   PIC  X(24)  VALUE  SPACE.
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SYK-ST             PIC  X(02).
 01  KON-ST             PIC  X(02).
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-TENCD           PIC  X(06).
 01  FILLER             REDEFINES      WK-TENCD.
     03  WK-H-TENCD     PIC  9(06).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-JDATE               PIC  9(08).
 01  PARA-IN-JTIME               PIC  9(04).
 01  PARA-IN-JTOKCD              PIC  9(08).
 01  PARA-IN-SOKO                PIC  X(02).
 01  PARA-IN-NOUDT               PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION USING       PARA-IN-JDATE
                                             PARA-IN-JTIME
                                             PARA-IN-JTOKCD
                                             PARA-IN-SOKO
                                             PARA-IN-NOUDT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カインズ出荷データ >>--*
 CZSYKTNF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZSYKTNF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1042B CZSYKTNF    ERROR " SYK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 梱包数集計ファイル >>--*
 CZKONGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZKONGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1042B CZKONGKF    ERROR " KON-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
*システム日付／時刻取得
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1042B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   HEN-DATE.
*ファイルＯＰＥＮ
     OPEN     INPUT     CZSYKTNF.
     OPEN     OUTPUT    CZKONGKF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         BRK-KEY.
*カインズ出荷データＳＴＡＲＴ
     MOVE     SPACE              TO   SYK-REC.
     INITIALIZE                       SYK-REC.
*
     MOVE     PARA-IN-JDATE      TO   SYK-F011.
     MOVE     PARA-IN-JTIME      TO   SYK-F012.
     MOVE     PARA-IN-JTOKCD     TO   SYK-F013.
     MOVE     PARA-IN-SOKO       TO   SYK-F02.
     MOVE     PARA-IN-NOUDT      TO   SYK-F349.
     START  CZSYKTNF  KEY  IS  >=     SYK-F011  SYK-F012
                                      SYK-F013  SYK-F02
                                      SYK-F349  SYK-F348
                                      SYK-F305  SYK-F309
                                      SYK-F604  SYK-F605
                                      SYK-F302  SYK-F402
            INVALID
            DISPLAY NC"＃対象データ無１！！＃＃" UPON  CONS
            MOVE    "END"        TO   END-FLG
            GO                   TO   INIT-RTN-EXIT
     END-START.
*カインズ出荷データ読込
     PERFORM  CZSYKTNF-READ-SEC.
*
     IF   END-FLG  =  "END"
          DISPLAY   NC"＃対象データ無２！！＃＃" UPON  CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　カインズ出荷データ読込　　　　　　　　　　　　　　　　　　*
****************************************************************
 CZSYKTNF-READ-SEC      SECTION.
*
 READ000.
*カインズ出荷データ読込
     READ  CZSYKTNF
           AT   END
            MOVE  "END"          TO        END-FLG
            GO                   TO        CZSYKTNF-READ-EXIT
     END-READ.
*****DISPLAY "F011 = " SYK-F011  UPON  CONS.
*    DISPLAY "F012 = " SYK-F012  UPON  CONS.
*    DISPLAY "F013 = " SYK-F013  UPON  CONS.
*    DISPLAY "F002 = " SYK-F02   UPON  CONS.
*****DISPLAY "F349 = " SYK-F349  UPON  CONS.

*読込件数カウント／件数表示
     ADD    1                    TO        IN-CNT.
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " IN-CNT  " #"  UPON CONS
     END-IF.
*受信日／時刻／取引先確認
 READ010.
*****DISPLAY "PARA-IN-JDATE   = " PARA-IN-JDATE  UPON CONS.
*    DISPLAY "PARA-IN-JTIME   = " PARA-IN-JTIME  UPON CONS.
*****DISPLAY "PARA-IN-JTOKCD  = " PARA-IN-JTOKCD UPON CONS.
     IF  PARA-IN-JDATE  =  SYK-F011
     AND PARA-IN-JTIME  =  SYK-F012
     AND PARA-IN-JTOKCD =  SYK-F013
         CONTINUE
     ELSE
         MOVE  "END"             TO        END-FLG
         GO                      TO        CZSYKTNF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
 READ020.
     IF  PARA-IN-SOKO  NOT =   SPACE
         IF   PARA-IN-SOKO  =  SYK-F02
              CONTINUE
         ELSE
              MOVE  "END"        TO        END-FLG
              GO                 TO        CZSYKTNF-READ-EXIT
         END-IF
     END-IF.
*納品日チェック
 READ030.
     IF  PARA-IN-NOUDT NOT =   ZERO
         IF  PARA-IN-NOUDT  =  SYK-F349
              CONTINUE
         ELSE
              MOVE  "END"        TO        END-FLG
              GO                 TO        CZSYKTNF-READ-EXIT
         END-IF
     END-IF.
*
*出荷数チェック
 READ040.
     IF  SYK-F458      NOT =   ZERO
         CONTINUE
     ELSE
         GO                      TO        READ000
     END-IF.
*------------------------------------------------------
     IF  TAISYO-CNT  =  ZERO
          MOVE    SYK-F011       TO        BRK-KEY-F011
          MOVE    SYK-F012       TO        BRK-KEY-F012
          MOVE    SYK-F013       TO        BRK-KEY-F013
          MOVE    SYK-F02        TO        BRK-KEY-F02
          MOVE    SYK-F349       TO        BRK-KEY-F349
          MOVE    SYK-F348       TO        BRK-KEY-F348
          MOVE    SYK-F305       TO        BRK-KEY-F305
**********MOVE    SYK-F605       TO        BRK-KEY-F605
     END-IF.
*
     ADD       1                 TO        TAISYO-CNT.
*
 CZSYKTNF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*バッチＮＯ、出荷場所、最終納品日、直接納品日、直接納品先
*ブレイク判定
*****DISPLAY "F011 = " BRK-KEY-F011 " - " SYK-F011   UPON CONS.
*    DISPLAY "F012 = " BRK-KEY-F012 " - " SYK-F012   UPON CONS.
*    DISPLAY "F013 = " BRK-KEY-F013 " - " SYK-F013   UPON CONS.
*    DISPLAY "F02  = " BRK-KEY-F02  " - " SYK-F02    UPON CONS.
*    DISPLAY "F349 = " BRK-KEY-F349 " - " SYK-F349   UPON CONS.
*    DISPLAY "F348 = " BRK-KEY-F348 " - " SYK-F348   UPON CONS.
*****DISPLAY "F305 = " BRK-KEY-F305 " - " SYK-F305   UPON CONS.
     IF   BRK-KEY-F011  =  SYK-F011
     AND  BRK-KEY-F012  =  SYK-F012
     AND  BRK-KEY-F013  =  SYK-F013
     AND  BRK-KEY-F02   =  SYK-F02
     AND  BRK-KEY-F349  =  SYK-F349
     AND  BRK-KEY-F348  =  SYK-F348
     AND  BRK-KEY-F305  =  SYK-F305
          CONTINUE
     ELSE
**********梱包数集計ファイル出力
          MOVE    SPACE          TO        KON-REC
          INITIALIZE                       KON-REC
          MOVE    BRK-KEY-F011   TO        KON-F01
          MOVE    BRK-KEY-F012   TO        KON-F02
          MOVE    BRK-KEY-F013   TO        KON-F03
          MOVE    BRK-KEY-F02    TO        KON-F04
          MOVE    BRK-KEY-F349   TO        KON-F05
          MOVE    BRK-KEY-F348   TO        KON-F06
*NG       MOVE    BRK-KEY-F305(1:6)  TO    WK-TENCD
*NG       MOVE    WK-H-TENCD     TO        KON-F07
          MOVE    BRK-KEY-F305(1:5)  TO    KON-F07
*
          MOVE    KON-CNT        TO        KON-F08
          WRITE   KON-REC
          ADD     1              TO        OUT-CNT
**********キーの入替
          MOVE    SYK-F011       TO        BRK-KEY-F011
          MOVE    SYK-F012       TO        BRK-KEY-F012
          MOVE    SYK-F013       TO        BRK-KEY-F013
          MOVE    SYK-F02        TO        BRK-KEY-F02
          MOVE    SYK-F349       TO        BRK-KEY-F349
          MOVE    SYK-F348       TO        BRK-KEY-F348
          MOVE    SYK-F305       TO        BRK-KEY-F305
          MOVE    ZERO           TO        KON-CNT
     END-IF.
*梱包ＮＯブレイクチェック
     IF   BRK-KEY-F605  NOT =  SYK-F605
          ADD     1              TO        KON-CNT
          MOVE    SYK-F605       TO        BRK-KEY-F605
     END-IF.
*
*カインズ出荷データ読込
     PERFORM  CZSYKTNF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*梱包カイントが０以上のとき、梱包数集計ファイルを出力する。
     IF   KON-CNT  >  ZERO
**********梱包数集計ファイル出力
          MOVE    SPACE          TO        KON-REC
          INITIALIZE                       KON-REC
          MOVE    BRK-KEY-F011   TO        KON-F01
          MOVE    BRK-KEY-F012   TO        KON-F02
          MOVE    BRK-KEY-F013   TO        KON-F03
          MOVE    BRK-KEY-F02    TO        KON-F04
          MOVE    BRK-KEY-F349   TO        KON-F05
          MOVE    BRK-KEY-F348   TO        KON-F06
*NG       MOVE    BRK-KEY-F305(1:6)  TO    WK-TENCD
*NG       MOVE    WK-H-TENCD     TO        KON-F07
          MOVE    BRK-KEY-F305(1:5)  TO    KON-F07
*
          MOVE    KON-CNT        TO        KON-F08
          WRITE   KON-REC
          ADD     1              TO        OUT-CNT
     END-IF.
*
     CLOSE        CZSYKTNF  CZKONGKF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUT-CNT    =" OUT-CNT " +++" UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1042B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
