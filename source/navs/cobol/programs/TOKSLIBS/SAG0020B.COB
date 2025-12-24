****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ユリックス売上作成　　　　　　　　*
*    モジュール名　　　　：　ユリックス売上作成　　　　　　　　*
*    作成日／更新日　　　：　05/04/01                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ユリックス売上データをサカタ売上　*
*                            へ変換する。　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SAG0020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/04/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   YURURISF  ASSIGN    TO        DA-01-S-YURURISF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    YUR-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*赤黒データ
     SELECT   AKADENF   ASSIGN    TO        AKADENF
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   AKA-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
 FD  YURURISF
                        BLOCK CONTAINS      30   RECORDS
                        LABEL RECORD   IS   STANDARD.
*
     COPY     YURURISF  OF        XFDLIB
              JOINING   YUR       PREFIX.
*
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  AKADENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   AKA       PREFIX.
*
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
*
 01  WK-ST.
     03  YUR-STATUS        PIC  X(02).
     03  AKA-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
 01  HATYU-HENKAN.
     03  HATYU-HEN-1       PIC  9(04).
     03  FILLER            PIC  X(01).
     03  HATYU-HEN-2       PIC  9(02).
     03  FILLER            PIC  X(01).
     03  HATYU-HEN-3       PIC  9(02).
 01  HATYU-HENKAN-DATE.
     03  HATYU-HEN-YYYY    PIC  9(04).
     03  HATYU-HEN-MM      PIC  9(02).
     03  HATYU-HEN-DD      PIC  9(02).
 01  HATYU-HENKAN-DATE-1   PIC  9(08)  VALUE  ZERO.
 01  NOUHN-HENKAN.
     03  NOUHN-HEN-1       PIC  9(04).
     03  FILLER            PIC  X(01).
     03  NOUHN-HEN-2       PIC  9(02).
     03  FILLER            PIC  X(01).
     03  NOUHN-HEN-3       PIC  9(02).
 01  NOUHN-HENKAN-DATE.
     03  NOUHN-HEN-YYYY    PIC  9(04).
     03  NOUHN-HEN-MM      PIC  9(02).
     03  NOUHN-HEN-DD      PIC  9(02).
 01  NOUHN-HENKAN-DATE-1   PIC  9(08)  VALUE  ZERO.
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SAG0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SAG0020B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SAG0020B".
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
*
 LINKAGE                SECTION.
 01  LINK-HIDUKE            PIC   9(08).
 01  LINK-JIKAN             PIC   9(04).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING LINK-HIDUKE LINK-JIKAN.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YURURISF.
     MOVE      "YURURISF"   TO   AB-FILE.
     MOVE      YUR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AKADENF.
     MOVE      "AKADENF "   TO   AB-FILE.
     MOVE      AKA-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     YURURISF
                        MEIMS1
                        TOKMS2.
     OPEN     OUTPUT    AKADENF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
*
     PERFORM  YURURISF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 YURURISF-READ-SEC     SECTION.
*
     READ     YURURISF
              AT END    MOVE      9         TO  END-FG
                        GO                  TO  YURURISF-READ-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3)  =  "000"  OR  "500"
          DISPLAY "READ-CNT = " RD-CNT UPON CONS
     END-IF.
*
     MOVE     YUR-F07         TO   NOUHN-HENKAN.
     MOVE     NOUHN-HEN-1     TO   NOUHN-HEN-YYYY.
     MOVE     NOUHN-HEN-2     TO   NOUHN-HEN-MM.
     MOVE     NOUHN-HEN-3     TO   NOUHN-HEN-DD.
*****DISPLAY "NOUHN-HENKAN-DATE = " NOUHN-HENKAN-DATE UPON CONS.
*
     MOVE     YUR-F06         TO   HATYU-HENKAN.
     MOVE     HATYU-HEN-1     TO   HATYU-HEN-YYYY.
     MOVE     HATYU-HEN-2     TO   HATYU-HEN-MM.
     MOVE     HATYU-HEN-3     TO   HATYU-HEN-DD.
 READ000.
*グッディーの３／２０までのデータは対象としない。
     IF       YUR-F01  =  28
     AND      NOUHN-HENKAN-DATE  <  20050320
              GO                            TO YURURISF-READ-SEC
     END-IF.
 READ010.
*得意先ＣＤが０は対象としない。
     IF       YUR-F01  =  0
              GO                            TO YURURISF-READ-SEC
     END-IF.
 READ020.
*店舗ＣＤが０は対象としない。
     IF       YUR-F04  =  0
              GO                            TO YURURISF-READ-SEC
     END-IF.
*
 YURURISF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     PERFORM  TENSO-SEC.
*
     PERFORM  YURURISF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*
     MOVE     SPACE        TO        AKA-REC.
     INITIALIZE                      AKA-REC.
*取引先コード
     MOVE     YUR-F01      TO        AKA-F01.
     MOVE     YUR-F01      TO        WK-TOKCD.
*伝票ナンバー
     MOVE     YUR-F02      TO        AKA-F02.
     MOVE     YUR-F02      TO        AKA-F23.
*行_
     MOVE     YUR-F03      TO        AKA-F03.
*取区
     EVALUATE  YUR-F05
         WHEN  11
         MOVE        40        TO        AKA-F051
         MOVE     NC"売上伝票" TO        AKA-F052
         WHEN  21
         MOVE        41        TO        AKA-F051
         MOVE     NC"売上返品" TO        AKA-F052
     END-EVALUATE.
*担当者コード
     MOVE        24        TO        AKA-F06.
*店コード
     MOVE        YUR-F04   TO        AKA-F07.
*出荷場所／伝発場所
     MOVE        "E1"      TO        AKA-F08
                                     AKA-F09.
*
     MOVE     YUR-F07         TO   NOUHN-HENKAN.
     MOVE     NOUHN-HEN-1     TO   NOUHN-HEN-YYYY.
     MOVE     NOUHN-HEN-2     TO   NOUHN-HEN-MM.
     MOVE     NOUHN-HEN-3     TO   NOUHN-HEN-DD.
     MOVE NOUHN-HENKAN-DATE   TO   NOUHN-HENKAN-DATE-1.
*
     MOVE     YUR-F06         TO   HATYU-HENKAN.
     MOVE     HATYU-HEN-1     TO   HATYU-HEN-YYYY.
     MOVE     HATYU-HEN-2     TO   HATYU-HEN-MM.
     MOVE     HATYU-HEN-3     TO   HATYU-HEN-DD.
     MOVE HATYU-HENKAN-DATE   TO   HATYU-HENKAN-DATE-1.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE NOUHN-HENKAN-DATE-1 TO       AKA-F111.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE NOUHN-HENKAN-DATE-1 TO       AKA-F112.
*出荷日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE        ZERO      TO        AKA-F113.
*分類（部門）
*商品区分　　
*伝票区分　　
*伝発区分
     MOVE        9         TO        AKA-F134.
*自社商品コード
     MOVE        YUR-F10   TO        AKA-F1411.
*自社商品単品コード
     MOVE        YUR-F11   TO        AKA-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE        YUR-F10   TO        MEI-F011.
     MOVE        YUR-F11   TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名
         MOVE    ALL "*"        TO   AKA-F1421
         MOVE    ALL "*"        TO   AKA-F1422
       NOT INVALID
         MOVE    MEI-F031       TO   AKA-F1421
         MOVE    MEI-F032       TO   AKA-F1422
     END-READ.
*数量
     MOVE        YUR-F12   TO        AKA-F15.
*単
     MOVE       "1"        TO        AKA-F16.
*原価単価
     MOVE        YUR-F13   TO        AKA-F172.
*売価単価
     MOVE        YUR-F15   TO        AKA-F173.
*原価金額
     MOVE        YUR-F14   TO        AKA-F181.
*売価金額
     MOVE        YUR-F16   TO        AKA-F182.
*店舗名（備考）
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    YUR-F01       TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F52   TO        AKA-F24
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F52   TO        AKA-F24
     END-IF.
*相手商品コード
     MOVE        YUR-F09   TO        AKA-F25.
*伝票発行区分
     MOVE        9         TO        AKA-F272.
*オンライン区分
     MOVE        0         TO        AKA-F274.
*エントリー区分
     MOVE        1         TO        AKA-F275.
*付番区分
     MOVE        9         TO        AKA-F276.
*量販店区分
     MOVE       "A"        TO        AKA-F278.
*ＷＳ_
     MOVE        1         TO        AKA-F28.
*変換値
*****MOVE        WK-NEN    TO        AKA-F29.
*店舗名（カナ）
*システム日付
     MOVE        SYS-DATEW TO        AKA-F99.
*ルート
*受信日付
     MOVE     LINK-HIDUKE  TO        AKA-F46.
*受信時刻
     MOVE     LINK-JIKAN   TO        AKA-F47.
*振分倉庫コード
*_番
*訂正前数量
     MOVE        YUR-F12   TO        AKA-F50.
*修正原価単価
     MOVE        YUR-F13   TO        AKA-F512.
*修正売価単価
     MOVE        YUR-F15   TO        AKA-F513.
*修正原価金額
     MOVE        YUR-F14   TO        AKA-F521.
*修正売価金額
     MOVE        YUR-F16   TO        AKA-F522.
     WRITE       AKA-REC.
     ADD         1         TO        WRT-CNT.
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
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     YURURISF  AKADENF
               TOKMS2    MEIMS1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
