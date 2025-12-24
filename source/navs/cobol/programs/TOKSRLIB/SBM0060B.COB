****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　流通ＢＭＳ　　　                  *
*　　モジュール名　　　　：　納品明細発行データ行数カウント　　*
*　　　　　　　　　　　　　　（ビバホーム）　　　　　　　　　　*
*　　作成日／作成者　　　：　2022/03/11  NAV                   *
*　　処理概要　　　　　　：　伝票発行ワーク中の伝票毎の行数を　*
*　　　　　　　　　　　　　　カウントし更新。　　　　　　　　  *
*　　　　　　　　　　　　　　後続の納品明細データ作成ＰＧ用。  *
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM0060B.
*                  流用:NKE1043B(カインズ）
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/03/11.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ビバホーム伝票発行ワーク >>--*
     SELECT   WKDENVH1  ASSIGN              DA-01-VI-WKDENVH1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   DEN-F011
                                                 DEN-F012
                                                 DEN-F013
                                                 DEN-F02
                                                 DEN-F308
                                                 DEN-F346
                                                 DEN-F342
                                                 DEN-F302
                                                 DEN-F402
                        WITH DUPLICATES
                        FILE      STATUS    IS   DEN-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ビバホーム伝票発行ワーク >>--*
 FD  WKDENVH1    LABEL RECORD   IS   STANDARD.
     COPY        WKDENVH1    OF      XFDLIB
                 JOINING     DEN     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
     03  INV-FLG        PIC  X(03)  VALUE  SPACE.
     03  NON-FLG        PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  RWT-CNT        PIC  9(07).
     03  GYO-CNT        PIC  9(02).
 01  BRK-WRK.
*   バッチ_受信日
     03  BRK-KEY-F011   PIC  9(08)     VALUE  ZERO.
*   バッチ_受信時刻
     03  BRK-KEY-F012   PIC  9(04)     VALUE  ZERO.
*   バッチ_取引先
     03  BRK-KEY-F013   PIC  9(08)     VALUE  ZERO.
*   出荷場所
     03  BRK-KEY-F02    PIC  X(02)     VALUE  SPACE.
*
 01  BRK-KEY.
*   最終納品先コード
     03  BRK-KEY-F308   PIC  X(13)     VALUE  SPACE.
*   最終納品先納品日
     03  BRK-KEY-F346   PIC  9(08)     VALUE  ZERO.
*   部門（商品分類（大））
     03  BRK-KEY-F342   PIC  X(10)     VALUE  SPACE.
*   伝票番号
     03  BRK-KEY-F302   PIC  X(10)     VALUE  SPACE.
*   行番号
     03  BRK-KEY-F402   PIC  X(04)     VALUE  SPACE.
*---------------------------------------------------
 01  WK-DEN-F303-X.
     03  WK-DEN-F303    PIC  9(02)     VALUE  ZERO.
     03  WK-DEN-F303FIL PIC  X(08)     VALUE  SPACE.
*---------------------------------------------------
 01  HEN-DATE           PIC  9(08)     VALUE  ZERO.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST             PIC  X(02).
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
*LINKAGE                SECTION.
*01  PARA-IN-JDATE               PIC  9(08).
*01  PARA-IN-JTIME               PIC  9(04).
*01  PARA-IN-JTOKCD              PIC  9(08).
*01  PARA-IN-SOKO                PIC  X(02).
*01  PARA-IN-NOUDT               PIC  9(08).
*01  PARA-OUT-KENSU              PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ビバホーム伝票発行ワーク >>--*
 WKDENVH1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      WKDENVH1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SBM0060B WKDENVH1    ERROR " DEN-ST " "
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
     DISPLAY  "*** SBM0060B START *** "
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
     OPEN     I-O       WKDENVH1.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         BRK-KEY.
*ビバホーム伝票発行ワークＳＴＡＲＴ
*    MOVE     SPACE              TO   DEN-REC.
*    INITIALIZE                       DEN-REC.
*
*    MOVE     PARA-IN-JDATE      TO   DEN-F011.
*    MOVE     PARA-IN-JTIME      TO   DEN-F012.
*    MOVE     PARA-IN-JTOKCD     TO   DEN-F013.
*    MOVE     PARA-IN-SOKO       TO   DEN-F02.
*    MOVE     PARA-IN-NOUDT      TO   DEN-F349.
*    START  WKDENVH1  KEY  IS  >=     DEN-F011  DEN-F012
*                                     DEN-F013  DEN-F02
*                                     DEN-F349  DEN-F309
*                                     DEN-F302  DEN-F402
*           INVALID
*           DISPLAY NC"＃対象データ無１！！＃＃" UPON  CONS
*           MOVE    "END"        TO   END-FLG
*           GO                   TO   INIT-RTN-EXIT
*    END-START.
*ビバホーム伝票発行ワーク読込
     PERFORM  WKDENVH1-READ-SEC.
*
     IF   END-FLG  =  "END"
          DISPLAY   NC"＃対象データ無　！！＃＃" UPON  CONS
          MOVE  "NON"      TO        NON-FLG
     ELSE
          MOVE  DEN-F011   TO   BRK-KEY-F011
          MOVE  DEN-F012   TO   BRK-KEY-F012
          MOVE  DEN-F013   TO   BRK-KEY-F013
          MOVE  DEN-F02    TO   BRK-KEY-F02
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　ビバホーム伝票発行ワーク読込　　　　　　　　　　　　　　　*
****************************************************************
 WKDENVH1-READ-SEC      SECTION.
*ビバホーム伝票発行ワーク読込
     READ  WKDENVH1  NEXT
           AT   END
            MOVE  "END"          TO        END-FLG
            GO                   TO        WKDENVH1-READ-EXIT
     END-READ.
*読込件数カウント／件数表示
     ADD    1                    TO        IN-CNT.
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " IN-CNT  " #"  UPON CONS
     END-IF.
*受信日／時刻／取引先確認
*    IF  PARA-IN-JDATE  =  DEN-F011
*    AND PARA-IN-JTIME  =  DEN-F012
*    AND PARA-IN-JTOKCD =  DEN-F013
*        CONTINUE
*    ELSE
*        MOVE  "END"             TO        END-FLG
*        GO                      TO        WKDENVH1-READ-EXIT
*    END-IF.
*倉庫ＣＤチェック
*    IF  PARA-IN-SOKO  NOT =   SPACE
*        IF   PARA-IN-SOKO  =  DEN-F02
*             CONTINUE
*        ELSE
*             MOVE  "END"        TO        END-FLG
*             GO                 TO        WKDENVH1-READ-EXIT
*        END-IF
*    END-IF.
*納品日チェック
*    IF  PARA-IN-NOUDT NOT =   ZERO
*        IF  PARA-IN-NOUDT  =  DEN-F349
*             CONTINUE
*        ELSE
*             MOVE  "END"        TO        END-FLG
*             GO                 TO        WKDENVH1-READ-EXIT
*        END-IF
*    END-IF.
*
 WKDENVH1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*店舗／納品日／分類／伝票番号　ブレイクチェック
     IF   BRK-KEY-F308  =  DEN-F308
     AND  BRK-KEY-F346  =  DEN-F346
     AND  BRK-KEY-F342  =  DEN-F342
     AND  BRK-KEY-F302  =  DEN-F302
          ADD     1              TO        GYO-CNT
     ELSE
          PERFORM WKDENVH1-START-SEC
*       　↓実際はINVALIDは発生しない
          IF      INV-FLG  =  "INV"
                  CLOSE    WKDENVH1
                  STOP     RUN
          END-IF
          PERFORM WKDENVH1-REWRITE-SEC
          MOVE    1              TO        GYO-CNT
          MOVE    DEN-F308       TO        BRK-KEY-F308
          MOVE    DEN-F346       TO        BRK-KEY-F346
          MOVE    DEN-F342       TO        BRK-KEY-F342
          MOVE    DEN-F302       TO        BRK-KEY-F302
     END-IF.
*ビバホーム伝票発行ワーク読込
     PERFORM  WKDENVH1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ビバホーム伝票発行ワーク　ＳＴＡＲＴ　　　　　　　　　　　*
****************************************************************
 WKDENVH1-START-SEC      SECTION.
*
     MOVE     SPACE              TO   DEN-REC.
     INITIALIZE                       DEN-REC.
*
     MOVE     BRK-KEY-F011       TO   DEN-F011.
     MOVE     BRK-KEY-F012       TO   DEN-F012.
     MOVE     BRK-KEY-F013       TO   DEN-F013.
     MOVE     BRK-KEY-F02        TO   DEN-F02.
     MOVE     BRK-KEY-F308       TO   DEN-F308.
     MOVE     BRK-KEY-F346       TO   DEN-F346.
     MOVE     BRK-KEY-F342       TO   DEN-F342.
     MOVE     BRK-KEY-F302       TO   DEN-F302.
     MOVE     "00  "             TO   DEN-F402.
     START  WKDENVH1  KEY  IS  >=     DEN-F011  DEN-F012  DEN-F013
                                      DEN-F02
                                      DEN-F308  DEN-F346
                                      DEN-F342
                                      DEN-F302  DEN-F402
         INVALID
            DISPLAY NC"＃対象データ無　？？＃＃" UPON  CONS
            MOVE    "INV"        TO   INV-FLG
         NOT INVALID
            MOVE    "HIT"        TO   INV-FLG
     END-START.
*
 WKDENVH1-START-EXIT.
     EXIT.
****************************************************************
*　　ビバホーム伝票発行ワーク　更新　　　　　　　　　　　　　　*
****************************************************************
 WKDENVH1-REWRITE-SEC      SECTION.
*
 WKDENVH1-REWRITE-01.
     READ  WKDENVH1  NEXT
           AT   END
            MOVE  "END"          TO        END-FLG
            GO                   TO        WKDENVH1-REWRITE-EXIT
     END-READ.
*
     IF   BRK-KEY-F011  =  DEN-F011  AND
          BRK-KEY-F012  =  DEN-F012  AND
          BRK-KEY-F013  =  DEN-F013  AND
          BRK-KEY-F02   =  DEN-F02   AND
          BRK-KEY-F308  =  DEN-F308  AND
          BRK-KEY-F346  =  DEN-F346  AND
          BRK-KEY-F342  =  DEN-F342  AND
          BRK-KEY-F302  =  DEN-F302
          CONTINUE
     ELSE
          GO               TO        WKDENVH1-REWRITE-EXIT
     END-IF.
*
*    伝票行数更新
     MOVE GYO-CNT          TO        WK-DEN-F303.
     MOVE WK-DEN-F303-X      TO        DEN-F303.
*
     REWRITE  DEN-REC.
     ADD      1            TO        RWT-CNT.
*
     GO                    TO        WKDENVH1-REWRITE-01.
*
 WKDENVH1-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
 END-RTN-01.
     IF      NON-FLG  =  "NON"
             GO       TO   END-RTN-02
     END-IF.
*    最終伝票分カウント値更新
     PERFORM WKDENVH1-START-SEC.
*    ↓実際はINVALIDは発生しない
     IF      INV-FLG  =  "INV"
             CLOSE    WKDENVH1
             STOP     RUN
     END-IF.
     PERFORM WKDENVH1-REWRITE-SEC.
*
 END-RTN-02.
     CLOSE              WKDENVH1.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ RWT-CNT    =" RWT-CNT " +++" UPON CONS.
*
*    MOVE     GYO-CNT         TO PARA-OUT-KENSU.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBM0060B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
