# NKE1041B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE1041B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　梱包数カウント　　　　　　　　　　*
*　　　　　　　　　　　　　　（カインズＴＣ１）　　　　　　　　*
*　　作成日／作成者　　　：　2019/06/26  NAV                   *
*　　処理概要　　　　　　：　出荷梱包送信データ＿出荷梱包リスト*
*　　　　　　　　　　　　　　（レコード区分Ｂ）の出荷総梱包数に*
*　　　　　　　　　　　　　　セットする直接納品先別梱包数をカウ*
*　　　　　　　　　　　　　　ントする。　　　　　　　　　　　　*
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE1041B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/26.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< カインズ検品結果データ >>--*
     SELECT   RCVCZDXX  ASSIGN              DA-01-S-RCVCZDXX
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   RCV-ST.
*----<< 検品紐付情報ファイル >>--*
     SELECT   CZKONXX1  ASSIGN              DA-01-VI-CZKONXX1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   KON-F01
                                                 KON-F02
                                                 KON-F03
                                                 KON-F04
                                                 KON-F05
                                                 KON-F06
                                                 KON-F07
                                                 KON-F08
                                                 KON-F09
                                                 KON-F10
                                                 KON-F11
                        FILE      STATUS    IS   KON-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< カインズ検品結果データ >>--*
 FD  RCVCZDXX    LABEL RECORD   IS   STANDARD
                 BLOCK CONTAINS 31   RECORDS.
     COPY        RCVCZDXX    OF      XFDLIB
                 JOINING     RCV     PREFIX.
*----<< 検品紐付情報ファイル >>--*
 FD  CZKONXX1    LABEL RECORD   IS   STANDARD.
     COPY        CZKONXX1    OF      XFDLIB
                 JOINING     KON     PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
     03  KON-INV        PIC  X(03)  VALUE  SPACE.
     03  KON-END        PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  TAISYO-CNT     PIC  9(07).
     03  KON-CNT        PIC  9(07).
     03  UPD-CNT        PIC  9(07).
 01  BRK-KEY.
     03  BRK-KEY-F01    PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F02    PIC  9(04)  VALUE  ZERO.
     03  BRK-KEY-F03    PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F04    PIC  X(02)  VALUE  SPACE.
     03  BRK-KEY-F05    PIC  9(08)  VALUE  ZERO.
     03  BRK-KEY-F06    PIC  9(05)  VALUE  ZERO.
*
     03  BRK-KEY-F09    PIC  X(24)  VALUE  SPACE.
*
*
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCV-ST             PIC  X(02).
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
*LINKAGE                SECTION.
*01  PARA-IN-JDATE               PIC  9(08).
*01  PARA-IN-JTIME               PIC  9(04).
*01  PARA-IN-JTOKCD              PIC  9(08).
*01  PARA-IN-SOKO                PIC  X(02).
*01  PARA-IN-NOUDT               PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< カインズ検品結果データ >>--*
 RCVCZDXX-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVCZDXX.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1041B RCVCZDXX    ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 検品紐付情報ファイル >>--*
 CZKONXX1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CZKONXX1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE1041B CZKONXX1    ERROR " KON-ST " "
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
     DISPLAY  "*** NKE1041B START *** "
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
     OPEN     INPUT     RCVCZDXX.
     OPEN     I-O       CZKONXX1.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         BRK-KEY.
*データ初期ＲＥＡＤ
     PERFORM  RCVCZDXX-READ-SEC.
*
     IF   END-FLG  =  "END"
          DISPLAY   NC"＃対象データなし！！＃＃" UPON  CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　カインズ検品結果データ読込
****************************************************************
 RCVCZDXX-READ-SEC      SECTION.
*
     READ  RCVCZDXX
           AT   END
            MOVE  "END"          TO        END-FLG
            GO                   TO        RCVCZDXX-READ-EXIT
     END-READ.
*
*読込件数カウント／件数表示
     ADD    1                    TO        IN-CNT.
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " IN-CNT  " #"  UPON CONS
     END-IF.
*
     IF  TAISYO-CNT  =  ZERO
          MOVE    RCV-F01        TO        BRK-KEY-F01
          MOVE    RCV-F02        TO        BRK-KEY-F02
          MOVE    RCV-F03        TO        BRK-KEY-F03
          MOVE    RCV-F04        TO        BRK-KEY-F04
          MOVE    RCV-F05        TO        BRK-KEY-F05
          MOVE    RCV-F06        TO        BRK-KEY-F06
     END-IF.
*
     ADD       1                 TO        TAISYO-CNT.
*
 RCVCZDXX-READ-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*集計単位ブレイク判定
*T↓
*    DISPLAY "F01 = " BRK-KEY-F01  " - " RCV-F01    UPON CONS.
*    DISPLAY "F02 = " BRK-KEY-F02  " - " RCV-F02    UPON CONS.
*    DISPLAY "F03 = " BRK-KEY-F03  " - " RCV-F03    UPON CONS.
*    DISPLAY "F04 = " BRK-KEY-F04  " - " RCV-F04    UPON CONS.
*    DISPLAY "F05 = " BRK-KEY-F05  " - " RCV-F05    UPON CONS.
*    DISPLAY "F06 = " BRK-KEY-F06  " - " RCV-F06    UPON CONS.
*T↑
*    非ブレイク
     IF ( BRK-KEY-F01   =  RCV-F01 )  AND
        ( BRK-KEY-F02   =  RCV-F02 )  AND
        ( BRK-KEY-F03   =  RCV-F03 )  AND
        ( BRK-KEY-F04   =  RCV-F04 )  AND
        ( BRK-KEY-F05   =  RCV-F05 )  AND
        ( BRK-KEY-F06   =  RCV-F06 )
          CONTINUE
*    ブレイク
     ELSE
*         検品紐付情報ファイル更新
          PERFORM KON-UPDT-SEC
*         キー入替
          MOVE    RCV-F01        TO        BRK-KEY-F01
          MOVE    RCV-F02        TO        BRK-KEY-F02
          MOVE    RCV-F03        TO        BRK-KEY-F03
          MOVE    RCV-F04        TO        BRK-KEY-F04
          MOVE    RCV-F05        TO        BRK-KEY-F05
          MOVE    RCV-F06        TO        BRK-KEY-F06
          MOVE    ZERO           TO        KON-CNT
     END-IF.
*梱包ＮＯブレイクチェック
     IF   BRK-KEY-F09   NOT =    RCV-F09
          ADD     1              TO        KON-CNT
          MOVE    RCV-F09        TO        BRK-KEY-F09
     END-IF.
*
*カインズ検品結果データ次ＲＥＡＤ
     PERFORM  RCVCZDXX-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       検品紐付情報ファイル更新　　                           *
*--------------------------------------------------------------*
 KON-UPDT-SEC            SECTION.
*
 KON-UPDT-01.
     MOVE    SPACE          TO        KON-REC.
     INITIALIZE                       KON-REC.
     MOVE    BRK-KEY-F01    TO        KON-F01.
     MOVE    BRK-KEY-F02    TO        KON-F02.
     MOVE    BRK-KEY-F03    TO        KON-F03.
     MOVE    BRK-KEY-F04    TO        KON-F04.
     MOVE    BRK-KEY-F05    TO        KON-F05.
     MOVE    BRK-KEY-F06    TO        KON-F06.
*
 KON-UPDT-02.
*ＳＴＡＲＴ
     PERFORM KON-START-SEC.
     IF      KON-INV   =   "INV"
             DISPLAY NC"更新先の紐付情報なし！？"  UPON CONS
             DISPLAY NC"バッチ（日付）＝"  BRK-KEY-F01 UPON CONS
             DISPLAY NC"バッチ（時刻）＝"  BRK-KEY-F02 UPON CONS
             DISPLAY NC"バッチ（取Ｃ）＝"  BRK-KEY-F03 UPON CONS
             DISPLAY NC"出荷場所ＣＤ　＝"  BRK-KEY-F04 UPON CONS
             DISPLAY NC"センター納品日＝"  BRK-KEY-F05 UPON CONS
             DISPLAY NC"センターＣＤ　＝"  BRK-KEY-F06 UPON CONS
             MOVE    "4010"           TO   PROGRAM-STATUS
             STOP    RUN
     END-IF.
*
 KON-UPDT-03.
*ＲＥＡＤ
     PERFORM KON-READ-SEC.
     IF      KON-END   =   "END"
             DISPLAY NC"更新先の紐付情報なし！？"  UPON CONS
             DISPLAY NC"バッチ（日付）＝"  BRK-KEY-F01 UPON CONS
             DISPLAY NC"バッチ（時刻）＝"  BRK-KEY-F02 UPON CONS
             DISPLAY NC"バッチ（取Ｃ）＝"  BRK-KEY-F03 UPON CONS
             DISPLAY NC"出荷場所ＣＤ　＝"  BRK-KEY-F04 UPON CONS
             DISPLAY NC"センター納品日＝"  BRK-KEY-F05 UPON CONS
             DISPLAY NC"センターＣＤ　＝"  BRK-KEY-F06 UPON CONS
             MOVE    "4010"           TO   PROGRAM-STATUS
             STOP    RUN
     END-IF.
*
 KON-UPDT-04.
*同一ＫＥＹレコード存在判定
*    存在
     IF ( BRK-KEY-F01   =   KON-F01 ) AND
        ( BRK-KEY-F02   =   KON-F02 ) AND
        ( BRK-KEY-F03   =   KON-F03 ) AND
        ( BRK-KEY-F04   =   KON-F04 ) AND
        ( BRK-KEY-F05   =   KON-F05 ) AND
        ( BRK-KEY-F06   =   KON-F06 )
          CONTINUE
*    非存在
     ELSE
          DISPLAY NC"更新先の紐付情報なし！？"  UPON CONS
          DISPLAY NC"バッチ（日付）＝"  BRK-KEY-F01 UPON CONS
          DISPLAY NC"バッチ（時刻）＝"  BRK-KEY-F02 UPON CONS
          DISPLAY NC"バッチ（取Ｃ）＝"  BRK-KEY-F03 UPON CONS
          DISPLAY NC"出荷場所ＣＤ　＝"  BRK-KEY-F04 UPON CONS
          DISPLAY NC"センター納品日＝"  BRK-KEY-F05 UPON CONS
          DISPLAY NC"センターＣＤ　＝"  BRK-KEY-F06 UPON CONS
          MOVE    "4010"           TO   PROGRAM-STATUS
          STOP    RUN
     END-IF.
*
*-----------------------------------------------------------------
 KON-UPDT-05.
*紐付情報ファイル更新
*
*出荷総梱包数
     MOVE    KON-CNT        TO        KON-F17.
*レコード更新
     REWRITE KON-REC.
     ADD     1              TO        UPD-CNT.
*
 KON-UPDT-06.
*次ＲＥＡＤ
     PERFORM KON-READ-SEC.
     IF      KON-END   =   "END"
             GO             TO        KON-UPDT-EXIT
     END-IF.
     IF ( BRK-KEY-F01   =   KON-F01 ) AND
        ( BRK-KEY-F02   =   KON-F02 ) AND
        ( BRK-KEY-F03   =   KON-F03 ) AND
        ( BRK-KEY-F04   =   KON-F04 ) AND
        ( BRK-KEY-F05   =   KON-F05 ) AND
        ( BRK-KEY-F06   =   KON-F06 )
          GO            TO            KON-UPDT-05
*    非存在
     ELSE
          GO            TO            KON-UPDT-EXIT
     END-IF.
*
 KON-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       検品紐付情報ファイルＳＴＡＲＴ                         *
*--------------------------------------------------------------*
 KON-START-SEC           SECTION.
*
 KON-START-01.
     START   CZKONXX1  KEY  >=        KON-F01
                                      KON-F02
                                      KON-F03
                                      KON-F04
                                      KON-F05
                                      KON-F06
                                      KON-F07
                                      KON-F08
                                      KON-F09
                                      KON-F10
                                      KON-F11
     INVALID
             MOVE   "INV"     TO      KON-INV
     NOT INVALID
             MOVE   "   "     TO      KON-INV
     END-START.
*
 KON-START-EXIT.
     EXIT.
*--------------------------------------------------------------*
*       検品紐付情報ファイルＲＥＡＤ　                         *
*--------------------------------------------------------------*
 KON-READ-SEC           SECTION.
*
 KON-READ-01.
*
     READ    CZKONXX1  NEXT
       AT END
             MOVE   "END"     TO      KON-END
       NOT AT END
             MOVE   "   "     TO      KON-END
*T↓
*      DISPLAY "KON-F01=" KON-F01 UPON CONS
*      DISPLAY "KON-F02=" KON-F02 UPON CONS
*      DISPLAY "KON-F03=" KON-F03 UPON CONS
*      DISPLAY "KON-F04=" KON-F04 UPON CONS
*      DISPLAY "KON-F05=" KON-F05 UPON CONS
*      DISPLAY "KON-F06=" KON-F06 UPON CONS
*      DISPLAY "KON-F07=" KON-F07 UPON CONS
*      DISPLAY "KON-F08=" KON-F08 UPON CONS
*      DISPLAY "KON-F09=" KON-F09 UPON CONS
*      DISPLAY "KON-F10=" KON-F10 UPON CONS
*      DISPLAY "KON-F11=" KON-F11 UPON CONS
*T↑
     END-READ.
*
 KON-READ-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*梱包カウントが０超のとき、検品紐付情報ファイル更新
     IF   KON-CNT  >  ZERO
          PERFORM KON-UPDT-SEC
     END-IF.
*
     CLOSE        RCVCZDXX  CZKONXX1.
*
     DISPLAY  NC"検品結果　読込＝" IN-CNT  UPON CONS.
     DISPLAY  NC"総梱包数　更新＝" UPD-CNT UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE1041B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
