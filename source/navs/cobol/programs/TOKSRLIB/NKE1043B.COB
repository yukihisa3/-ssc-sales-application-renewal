****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　伝票枚数カウント　　　　　　　　　*
*　　　　　　　　　　　　　　（カインズ）　　　　　　　　　　　*
*　　作成日／作成者　　　：　2019/06/24  NAV                   *
*　　処理概要　　　　　　：　作成対象データ中の伝票枚数をカウ　*
*　　　　　　　　　　　　　　ント。送信データ　取引件数　　　  *
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE1043B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/24.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< カインズ出荷データ >>--*
     SELECT   CZSYKTCF  ASSIGN              DA-01-VI-CZSYKTC3
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   SYK-F011
                                                 SYK-F012
                                                 SYK-F013
                                                 SYK-F02
                                                 SYK-F349
                                                 SYK-F309
                                                 SYK-F302
                                                 SYK-F402
                        FILE      STATUS    IS   SYK-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< カインズ出荷データ >>--*
 FD  CZSYKTCF    LABEL RECORD   IS   STANDARD.
     COPY        CZSYKTCF    OF      XFDLIB
                 JOINING     SYK     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  DEN-CNT        PIC  9(07).
 01  BRK-KEY.
     03  BRK-KEY-F309   PIC  X(13)  VALUE  SPACE.
     03  BRK-KEY-F302   PIC  X(10)  VALUE  SPACE.
     03  BRK-KEY-F402   PIC  X(04)  VALUE  SPACE.
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SYK-ST             PIC  X(02).
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
 01  PARA-OUT-KENSU              PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION USING       PARA-IN-JDATE
                                             PARA-IN-JTIME
                                             PARA-IN-JTOKCD
                                             PARA-IN-SOKO
                                             PARA-IN-NOUDT
                                             PARA-OUT-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カインズ出荷データ >>--*
 CZSYKTCF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZSYKTCF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1043B CZSYKTCF    ERROR " SYK-ST " "
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
     DISPLAY  "*** NKE1043B START *** "
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
     OPEN     INPUT     CZSYKTCF.
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
     START  CZSYKTCF  KEY  IS  >=     SYK-F011  SYK-F012
                                      SYK-F013  SYK-F02
                                      SYK-F349  SYK-F309
                                      SYK-F302  SYK-F402
            INVALID
            DISPLAY NC"＃対象データ無１！！＃＃" UPON  CONS
            MOVE    "END"        TO   END-FLG
            GO                   TO   INIT-RTN-EXIT
     END-START.
*カインズ出荷データ読込
     PERFORM  CZSYKTCF-READ-SEC.
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
 CZSYKTCF-READ-SEC      SECTION.
*カインズ出荷データ読込
     READ  CZSYKTCF
           AT   END
            MOVE  "END"          TO        END-FLG
            GO                   TO        CZSYKTCF-READ-EXIT
     END-READ.
*読込件数カウント／件数表示
     ADD    1                    TO        IN-CNT.
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " IN-CNT  " #"  UPON CONS
     END-IF.
*受信日／時刻／取引先確認
     IF  PARA-IN-JDATE  =  SYK-F011
     AND PARA-IN-JTIME  =  SYK-F012
     AND PARA-IN-JTOKCD =  SYK-F013
         CONTINUE
     ELSE
         MOVE  "END"             TO        END-FLG
         GO                      TO        CZSYKTCF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF  PARA-IN-SOKO  NOT =   SPACE
         IF   PARA-IN-SOKO  =  SYK-F02
              CONTINUE
         ELSE
              MOVE  "END"        TO        END-FLG
              GO                 TO        CZSYKTCF-READ-EXIT
         END-IF
     END-IF.
*納品日チェック
     IF  PARA-IN-NOUDT NOT =   ZERO
         IF  PARA-IN-NOUDT  =  SYK-F349
              CONTINUE
         ELSE
              MOVE  "END"        TO        END-FLG
              GO                 TO        CZSYKTCF-READ-EXIT
         END-IF
     END-IF.
*
 CZSYKTCF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*店舗／伝票番号／行番号ブレイクチェック
     IF   BRK-KEY-F309  =  SYK-F309
     AND  BRK-KEY-F302  =  SYK-F302
          CONTINUE
     ELSE
          ADD     1              TO        DEN-CNT
          MOVE    SYK-F309       TO        BRK-KEY-F309
          MOVE    SYK-F302       TO        BRK-KEY-F302
     END-IF.
*カインズ出荷データ読込
     PERFORM  CZSYKTCF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE              CZSYKTCF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ DEN-CNT    =" DEN-CNT " +++" UPON CONS.
*
     MOVE     DEN-CNT         TO PARA-OUT-KENSU.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1043B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
