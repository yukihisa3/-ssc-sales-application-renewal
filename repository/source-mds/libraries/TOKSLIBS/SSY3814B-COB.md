# SSY3814B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3814B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援                    *
*    モジュール名　　　　：　オンライン作場振分情報リスト
*    　　　　　　　　　　　　（データ抽出）
*    作成日／作成者　　　：　2015/04/28 INOUE                  *
*    処理概要　　　　　　：　該当バッチ_のデータについて、    *
*                            振り分けられた作場毎の発注明細　　*
*                            を抽出する。　                    *
*　　更新日／更新者　　　：　2016/08/23 INOUE                  *
*    修正概要　　　　　　：　本発ＥＤＩ対応　　　　　　        *
*　　更新日／更新者　　　：　2016/09/15 TAKAHASHI              *
*    修正概要　　　　　　：　本発ＥＤＩ対応（納品日、集約追加）*
*　　更新日／更新者　　　：　2016/09/20 INOUE                  *
*　　修正概要　　　　　　：　項目追加（入数）                  *
*　　更新日／更新者　　　：　2016/10/03 TAKAHASHI              *
*　　修正概要　　　　　　：　集約＝店Ｐ無ＯＫ　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3814B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/04/28.
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
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*振分リスト出力ワーク
     SELECT   HURXXXW  ASSIGN    TO        DA-01-S-HURXXXW
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HUR-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*管理番号ファイル
     SELECT   NFKANRL2  ASSIGN    TO        DA-01-VI-NFKANRL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KAN-F01   KAN-F03
                        FILE      STATUS    IS   KAN-STATUS.
*↓2016/08/23
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F02
                        FILE      STATUS    IS   SHO-STATUS.
*ナフコ店舗パターンマスタ
     SELECT   NFTEPTL1  ASSIGN    TO        DA-01-VI-NFTEPTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEP-F01
                                                 TEP-F02
                        FILE      STATUS    IS   TEP-STATUS.
*↑2016/08/23
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    発注集計表データファイル
******************************************************************
 FD  HURXXXW           BLOCK     CONTAINS  15   RECORDS.
     COPY     HURXXXW  OF        XFDLIB
              JOINING   HUR       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    管理番号ファイル
******************************************************************
 FD  NFKANRL2           LABEL RECORD   IS   STANDARD.
     COPY     NFKANRL2  OF        XFDLIB
              JOINING   KAN       PREFIX.
*↓2016/08/23
******************************************************************
*    ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO       PREFIX.
*
******************************************************************
*    ナフコ店舗パターンマスタ
******************************************************************
 FD  NFTEPTL1           LABEL RECORD   IS   STANDARD.
     COPY     NFTEPTL1  OF        XFDLIB
              JOINING   TEP       PREFIX.
*
*↑2016/08/23
*****************************************************************
 WORKING-STORAGE        SECTION.
*ナフコ伝票データレイアウト
     COPY   NFSHIRED OF XFDLIB  JOINING   NFD  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HUR-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  KAN-STATUS        PIC  X(02).
*↓2016/08/23
     03  SHO-STATUS        PIC  X(02).
     03  TEP-STATUS        PIC  X(02).
*↑2016/08/23
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3814B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3814B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3814B".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-SAKUBAF           PIC   X(02).
 01  PARA-SAKUBAT           PIC   X(02).
*↓2016/08/23
 01  PARA-SYOBUNF           PIC   X(08).
 01  PARA-SYOBUNT           PIC   X(08).
 01  PARA-PATNF             PIC   X(02).
 01  PARA-PATNT             PIC   X(02).
*↑2016/08/23
*↓2016/09/15
 01  PARA-NOUHDT            PIC   9(08).
 01  PARA-SYKKBN            PIC   X(01).
*↑2016/09/15
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*↓2016/08/23
*PROCEDURE              DIVISION USING PARA-JDATE
*                                      PARA-JTIME
*                                      PARA-TOKCD
*                                      PARA-SAKUBAF
*                                      PARA-SAKUBAT.
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TOKCD
                                       PARA-SAKUBAF
                                       PARA-SAKUBAT
                                       PARA-SYOBUNF
                                       PARA-SYOBUNT
                                       PARA-PATNF
*↓2016/09/15
***************************************PARA-PATNT.
                                       PARA-PATNT
                                       PARA-NOUHDT
                                       PARA-SYKKBN.
*↑2016/09/15
*↑2016/08/23
 DECLARATIVES.
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
                        PROCEDURE   HURXXXW.
     MOVE      "HURXXXW "  TO   AB-FILE.
     MOVE      HUR-STATUS   TO   AB-STS.
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
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1"    TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFKANRL2.
     MOVE      "NFKANRL2"   TO   AB-FILE.
     MOVE      KAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*↓2016/08/23
*
 FILEERR-SEC7           SECTION.
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
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFTEPTL1.
     MOVE      "NFTEPTL1"   TO   AB-FILE.
     MOVE      TEP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*↑2016/08/23
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
     OPEN     INPUT     SHTDENLA TOKMS2 SHOTBL1 JYOKEN1 NFKANRL2.
     OPEN     OUTPUT    HURXXXW.
*↓2016/08/23
     OPEN     INPUT     NFSHOMS1 NFTEPTL1.
*↑2016/08/23
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*    得意先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TOKCD     TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*    条件ファイル検索
     MOVE     SPACE          TO   JYO-REC
     INITIALIZE                   JYO-REC
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZEI"          TO   JYO-F02.
     READ     JYOKEN1
        INVALID
              DISPLAY NC"消費税率取得エラー！！" UPON CONS
              MOVE 4000      TO   PROGRAM-STATUS
              STOP  RUN
        NOT INVALID
*                     項目変更
              MOVE JYO-F04   TO   WK-ZEIRITU1
              MOVE JYO-F05   TO   WK-ZEIRITU2
              MOVE JYO-F06   TO   WK-ZEIKAITEI
              MOVE JYO-F07   TO   WK-ZEIRITU3
              MOVE JYO-F08   TO   WK-ZEIRITU4
     END-READ.
*
*    管理番号ファイル検索
     MOVE     1              TO    KAN-F01.
     MOVE     PARA-JDATE     TO    KAN-F031.
     MOVE     PARA-JTIME     TO    KAN-F032.
     MOVE     PARA-TOKCD     TO    KAN-F033.
     READ     NFKANRL2
       INVALID
              DISPLAY NC"管理番号取得エラー！！" UPON CONS
              MOVE 4000      TO   PROGRAM-STATUS
              STOP  RUN
     END-READ.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TOKCD     TO   DEN-F01.
     MOVE     PARA-SAKUBAF   TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TOKCD     =    DEN-F01 )
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*    倉庫ＣＤ範囲チェック
     IF       PARA-SAKUBAF     NOT =     SPACE
              IF ( DEN-F48   >=   PARA-SAKUBAF ) AND
                 ( DEN-F48   <=   PARA-SAKUBAT )
                   CONTINUE
              ELSE
                   GO   TO        MAIN-010
              END-IF
     ELSE
              CONTINUE
     END-IF.
*
*↓2016/08/23
*    商品分類ＣＤ範囲チェック
     MOVE     DEN-F25        TO   SHO-F02.
     READ     NFSHOMS1
          INVALID
              DISPLAY NC"ナフコ商品マスタなし！" UPON CONS
              DISPLAY NC"商品ＣＤ　＝" DEN-F25   UPON CONS
              MOVE    4010    TO   PROGRAM-STATUS
              STOP    RUN
     END-READ.
     IF       PARA-SYOBUNF     NOT =     SPACE
              IF   SHO-F45   >=   PARA-SYOBUNF
                   CONTINUE
              ELSE
                   GO   TO        MAIN-010
              END-IF
     END-IF.
     IF       PARA-SYOBUNT     NOT =     SPACE
              IF   SHO-F45   <=   PARA-SYOBUNT
                   CONTINUE
              ELSE
                   GO   TO        MAIN-010
              END-IF
     END-IF.
*
*    パターンＣＤ範囲チェック
     MOVE     DEN-F07        TO   TEP-F01.
     MOVE     SHO-F45        TO   TEP-F02.
     READ     NFTEPTL1
          INVALID
              DISPLAY NC"ナフコ店舗パターンマスタなし！"
                                                 UPON CONS
              DISPLAY NC"店舗ＣＤ　　　＝" DEN-F07   UPON CONS
              DISPLAY NC"商品分類ＣＤ　＝" SHO-F45   UPON CONS
          DISPLAY NC"店パターンエラー" ": TENCD = " DEN-F07
                    " BUNCD = " SHO-F45
**************2016/10/03 NAV ST 集約の場合は異常終了させない
              IF  PARA-SYKKBN  NOT =  "1"
                  MOVE    4010    TO   PROGRAM-STATUS
                  STOP    RUN
              END-IF
**************2016/10/03 NAV ED 集約の場合は異常終了させない
     END-READ.
     IF       PARA-PATNF       NOT =     SPACE
              IF   TEP-F03   >=   PARA-PATNF
                   CONTINUE
              ELSE
                   GO   TO        MAIN-010
              END-IF
     END-IF.
     IF       PARA-PATNT       NOT =     SPACE
              IF   TEP-F03   <=   PARA-PATNT
                   CONTINUE
              ELSE
                   GO   TO        MAIN-010
              END-IF
     END-IF.
*↑2016/08/23
*↑2016/09/15
     IF   PARA-NOUHDT  NOT =  ZERO
          MOVE  DEN-F112         TO    WK-DEN-F112
          IF  WK-DEN-F112  =  PARA-NOUHDT
              CONTINUE
          ELSE
              GO        TO        MAIN-010
          END-IF
     END-IF.
*↑2016/09/15
*
*売上伝票データをワークナフコ伝票データにセット
     MOVE     DEN-REC        TO   NFD-REC.
*振分リスト出力ワーク出力
     MOVE     SPACE          TO   HUR-REC.
     INITIALIZE                   HUR-REC.
*受信日付
     MOVE     DEN-F46        TO   HUR-F011.
*受信時刻
     MOVE     DEN-F47        TO   HUR-F012.
*受信取引先
     MOVE     DEN-F01        TO   HUR-F013.
*店舗コード
     MOVE     DEN-F07        TO   HUR-F02.
*作場（倉庫）コード
     MOVE     DEN-F48        TO   HUR-F03.
*ルート
     MOVE     DEN-F42        TO   HUR-F20.
*部門（分類）コード
     MOVE     DEN-F12        TO   HUR-F04.
*発注日
     MOVE     DEN-F111       TO   HUR-F05.
*納品日
     MOVE     DEN-F112       TO   HUR-F06.
*相手商品コード
     MOVE     DEN-F25        TO   HUR-F07.
*自社商品コード
     MOVE     DEN-F1411      TO   HUR-F081.
*品単コード
     MOVE     DEN-F1412      TO   HUR-F082.
*棚番
*  商品変換テーブル検索
     MOVE     DEN-F01        TO    TBL-F01.
     MOVE     DEN-F25        TO    TBL-F02.
     READ    SHOTBL1
       INVALID
*棚番
              MOVE  SPACE    TO    HUR-F09
       NOT INVALID
*棚番 自社商品コード 品単コード
              MOVE  TBL-F08  TO    HUR-F09
              MOVE  TBL-F031 TO    HUR-F081
              MOVE  TBL-F032 TO    HUR-F082
     END-READ.
*商品区分
     MOVE     SPACE          TO   HUR-F10.
*商品名１
     MOVE     DEN-F1421      TO   HUR-F111.
*商品名２
     MOVE     DEN-F1422      TO   HUR-F112.
*規格
     MOVE     SPACE          TO   HUR-F12.
*受信した商品名をセット2015/06/17追加
     IF  NFD-A49 NOT = SPACE
         MOVE NFD-A49        TO   HUR-F11
     END-IF.
*受信した規格名をセット2015/06/17追加
     IF  NFD-A57 NOT = SPACE
         MOVE NFD-A57        TO   HUR-F12
     END-IF.
*発注数量
     MOVE     DEN-F50        TO   HUR-F13.
*訂正後数量
     MOVE     DEN-F15        TO   HUR-F14.
*原価単価 ※訂正前
     MOVE     DEN-F512       TO   HUR-F15.
*売価単価 ※訂正前
     MOVE     DEN-F513       TO   HUR-F16.
*原価金額 ※訂正前
     MOVE     DEN-F521       TO   HUR-F18.
*売価金額 ※訂正前
     MOVE     DEN-F522       TO   HUR-F19.
*発注企業者名
     MOVE     TOK-F04        TO   HUR-F17.
*
*    IF RD-CNT(6:3) = "000" OR "500"
*      DISPLAY "TOK-F88 = " TOK-F88 UPON CONS
*      DISPLAY "TOK-F97 = " TOK-F97 UPON CONS
*    END-IF.
*税込売価
     IF       TOK-F97  =  "0"
      IF  WK-ZEIKAITEI NOT = ZERO    *>改訂日が０以上
       IF WK-ZEIKAITEI <= DEN-F112   *>納日が改訂日以上、新税率
          EVALUATE      TOK-F88
              WHEN      "0"
              COMPUTE   HUR-F21 = HUR-F16 * WK-ZEIRITU3
              WHEN      "4"
              COMPUTE   HUR-F21 ROUNDED = HUR-F16 * WK-ZEIRITU3
              WHEN      "9"
              COMPUTE   HUR-F21 = HUR-F16 * WK-ZEIRITU3 + 0.99
              WHEN      OTHER
              MOVE      HUR-F16     TO        HUR-F21
          END-EVALUATE
       ELSE                          *>納日が改訂日以下、旧税率
          EVALUATE      TOK-F88
              WHEN      "0"
              COMPUTE   HUR-F21 = HUR-F16 * WK-ZEIRITU1
              WHEN      "4"
              COMPUTE   HUR-F21 ROUNDED = HUR-F16 * WK-ZEIRITU1
              WHEN      "9"
              COMPUTE   HUR-F21 = HUR-F16 * WK-ZEIRITU1 + 0.99
              WHEN      OTHER
              MOVE      HUR-F16     TO        HUR-F21
          END-EVALUATE
       END-IF
      ELSE
          EVALUATE      TOK-F88      *>改訂日が０の場合、旧税率
              WHEN      "0"
              COMPUTE   HUR-F21 = HUR-F16 * WK-ZEIRITU1
              WHEN      "4"
              COMPUTE   HUR-F21 ROUNDED = HUR-F16 * WK-ZEIRITU1
              WHEN      "9"
              COMPUTE   HUR-F21 = HUR-F16 * WK-ZEIRITU1 + 0.99
              WHEN      OTHER
              MOVE      HUR-F16     TO        HUR-F21
          END-EVALUATE
      END-IF
     ELSE
              MOVE           HUR-F16    TO        HUR-F21
     END-IF.
*管理番号
     MOVE     KAN-F02        TO   HUR-F23.
*↓2016/08/23
*商品分類ＣＤ
     MOVE     SHO-F45        TO   HUR-F24.
*パターンＣＤ
     MOVE     TEP-F03        TO   HUR-F25.
*↑2016/08/23
*
*↓2016/09/15
     IF  PARA-SYKKBN  =  "1"
         MOVE SPACE          TO   HUR-F24
         MOVE SPACE          TO   HUR-F25
     END-IF.
*↑2016/09/15
*↓2016/09/20
*ＪＡＮＣＤ
     MOVE     SHO-F04        TO   HUR-F26.
*入数
     MOVE     SHO-F09        TO   HUR-F27.
*↑2016/09/20
     WRITE    HUR-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                   ADD  1    TO   RD-CNT
     END-READ.
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
     CLOSE     SHTDENLA  TOKMS2  HURXXXW  SHOTBL1  NFKANRL2.
*↓2016/08/23
     CLOSE     NFSHOMS1  NFTEPTL1.
*↑2016/08/23
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
