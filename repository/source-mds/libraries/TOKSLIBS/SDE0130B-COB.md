# SDE0130B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDE0130B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ取込　　　　　　　　*
*    モジュール名　　　　：　オンライン作場振分情報リスト
*    　　　　　　　　　　　　（データ抽出）
*    作成日／作成者　　　：　2016/09/26 TAKAHASHI              *
*    処理概要　　　　　　：　パラメタで受け取った内容で、基本  *
*                            売上伝票データより対象データの抽　*
*                            出する。　　　                    *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SDE0130B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/26.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本売上伝票データ
     SELECT   URIXXXL2  ASSIGN    TO        DA-01-VI-URIXXXL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       URI-F01   URI-F051
                                            URI-F07   URI-F02
                                            URI-F03
                        FILE  STATUS   IS   URI-STATUS.
*発注集計表出力ワーク
     SELECT   HACXXXF   ASSIGN    TO        DA-01-S-HACXXXF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HAC-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本売上伝票データ
******************************************************************
 FD  URIXXXL2
                        LABEL RECORD   IS   STANDARD.
     COPY     URIXXXF   OF        XFDLIB
              JOINING   URI  AS   PREFIX.
*
******************************************************************
*    発注集計表データファイル
******************************************************************
 FD  HACXXXF           BLOCK     CONTAINS  15   RECORDS.
     COPY     HACXXXF  OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*****************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-URI-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  URI-STATUS        PIC  X(02).
     03  HAC-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SDE0130B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0130B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0130B".
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
*税計算用
*$$2013/12/12 NAV ST 消費税増税対応
 01  WK-ZEIRITU             PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU1            PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU2            PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIKAITEI           PIC   9(08)     VALUE  ZERO.
 01  WK-ZEIRITU3            PIC   9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU4            PIC   9(01)V99  VALUE  ZERO.
*$$2013/12/12 NAV ED 消費税増税対応
*
 LINKAGE                SECTION.
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-TENST             PIC   9(05).
 01  PARA-TENED             PIC   9(05).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*↓2016/08/23
 PROCEDURE              DIVISION USING PARA-TOKCD
                                       PARA-TENST
                                       PARA-TENED.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   URIXXXL2.
     MOVE      "URIXXXL2"   TO   AB-FILE.
     MOVE      URI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HACXXXF.
     MOVE      "HACXXXF "   TO   AB-FILE.
     MOVE      HAC-STATUS   TO   AB-STS.
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
     OPEN     INPUT     URIXXXL2 TOKMS2.
     OPEN     OUTPUT    HACXXXF.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    取引先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TOKCD     TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*基本売上伝票データスタート
     MOVE     SPACE          TO   URI-REC.
     INITIALIZE                   URI-REC.
     MOVE     PARA-TOKCD     TO   URI-F01.
     MOVE     40             TO   URI-F051.
     MOVE     PARA-TENST     TO   URI-F07.
     START    URIXXXL2  KEY  >=   URI-F01   URI-F051
                                  URI-F07   URI-F02
                                  URI-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     PERFORM  URIXXXL2-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 URIXXXL2-READ-SEC    SECTION.
*
     READ     URIXXXL2
              AT END    MOVE      9         TO  END-FG
                        GO                  TO  URIXXXL2-READ-EXIT
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
*行番号＝８０は対象外
     IF  URI-F03  =  80
         GO                      TO  URIXXXL2-READ-SEC
     END-IF.
*パラ：取引先ＣＤ＝０の場合、全ての取引先対象
     IF  PARA-TOKCD  =  ZERO
         CONTINUE
     ELSE
         IF  PARA-TOKCD  <  URI-F01
             MOVE      9         TO  END-FG
             GO                  TO  URIXXXL2-READ-EXIT
         END-IF
     END-IF.
*伝区チェック（４０以外はよも飛ばし）
     IF  URI-F051  NOT =  40
         GO                      TO  URIXXXL2-READ-SEC
     END-IF.
*店舗ＣＤ範囲チェック
     IF  PARA-TENST  <=  URI-F07
     AND PARA-TENED  >=  URI-F07
         CONTINUE
     ELSE
         GO                      TO  URIXXXL2-READ-SEC
     END-IF.
*
 URIXXXL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*発注集計表出力ワーク出力
     MOVE     SPACE               TO   HAC-REC.
     INITIALIZE                        HAC-REC.
*受信日付
     MOVE     URI-F62             TO   HAC-F011.
*受信時刻
     MOVE     URI-F67(1:4)        TO   HAC-F012.
*受信取引先
     MOVE     URI-F01             TO   HAC-F013.
*店舗コード
     MOVE     URI-F07             TO   HAC-F02.
*作場（倉庫）コード
     MOVE     URI-F48             TO   HAC-F03.
*ルート
     MOVE     URI-F42             TO   HAC-F20.
*部門（分類）コード
     MOVE     URI-F12             TO   HAC-F04.
*発注日
     MOVE     URI-F111            TO   HAC-F05.
*納品日
     MOVE     URI-F112            TO   HAC-F06.
*相手商品コード
     MOVE     URI-F25             TO   HAC-F07.
*自社商品コード
     MOVE     URI-F1411           TO   HAC-F081.
*品単コード
     MOVE     URI-F1412           TO   HAC-F082.
*棚番
     MOVE     SPACE               TO   HAC-F09.
*商品区分
     MOVE     SPACE               TO   HAC-F10.
*商品名１
     MOVE     URI-F1421           TO   HAC-F111.
*商品名２
     MOVE     URI-F1422           TO   HAC-F112.
*規格
     MOVE     SPACE               TO   HAC-F12.
*発注数量
     MOVE     URI-F50             TO   HAC-F13.
*訂正後数量
     MOVE     URI-F15             TO   HAC-F14.
*原価単価 ※訂正前
     MOVE     URI-F512            TO   HAC-F15.
*売価単価 ※訂正前
     MOVE     URI-F513            TO   HAC-F16.
*原価金額 ※訂正前
     MOVE     URI-F521            TO   HAC-F18.
*売価金額 ※訂正前
     MOVE     URI-F522            TO   HAC-F19.
*発注企業者名
     MOVE     TOK-F04             TO   HAC-F17.
*税込売価
     MOVE     ZERO                TO   HAC-F21.
*小売連携区分
     MOVE     SPACE               TO   HAC-F22.
*
     WRITE    HAC-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     PERFORM URIXXXL2-READ-SEC.
*
 MAIN-EXIT.
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
     IF  OUT-CNT  =  ZERO
         DISPLAY NC"＃＃抽出対象がありません！！＃＃" UPON CONS
     END-IF.
*
     CLOSE     URIXXXL2  TOKMS2  HACXXXF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
