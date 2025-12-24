# SNA0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0010I.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：小売連携発注データ抽出指示
*    作成日／更新日　：2011/10/27
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      苗業務システムへ送信する小売連携（オンライン）
*      発注データを、売上伝票ファイルより抽出するため
*      の条件指定。
*      チェックＯＫなら条件をパラメータに出力する。
*    変更日／更新日　：2012/07/12
*    変更内容　　　　：
*      売上伝票データの小売連携区分はノーチェックにし、
*      倉庫マスタの小売連携区分をチェックする。
*    変更日／更新日　：2012/09/21
*    変更内容　　　　：
*      データ存在チェックに、出荷数量＞０以上のチェックを
*      追加する。
*    変更日／更新日　：2013/08/19
*    変更内容　　　　：
*      実納品日のチェック方法の障害を修正
*      （末日算出方法の変更）
*    変更日／更新日　：2016/10/20
*    変更内容　　　　：
*      小売連携商品のみを対象とする
*      ２０１２／０７／１２対応分の戻し作業
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0010I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/10/27.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
     SELECT  DSPFILE
       ASSIGN    TO        GS-DSPF
       FORMAT              DSP-FMT
       GROUP               DSP-GRP
       PROCESSING          DSP-PRO
       FUNCTION            DSP-FNC
       FILE      STATUS    DSP-ST.

*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN    TO        SHTDENLA
       ORGANIZATION        INDEXED
       ACCESS    MODE      SEQUENTIAL
       RECORD    KEY
         URI-F461 *> 受信日付（年）
         URI-F462 *> 受信日付（月）
         URI-F463 *> 受信日付（日）
         URI-F471 *> 受信時間（時）
         URI-F472 *> 受信時間（分）
         URI-F01  *> 取引先コード
         URI-F48  *> 振分倉庫
         URI-F02  *> 伝票番号
         URI-F04  *> 相殺区分
         URI-F051 *> 伝区コード
         URI-F07  *> 店舗コード
         URI-F112 *> 納品日
         URI-F03  *> 行番号
       FILE      STATUS    URI-ST.

*倉庫マスタ
     SELECT  ZSOKMS
       ASSIGN    TO        ZSOKMS1
       ORGANIZATION        INDEXED
       ACCESS    MODE      RANDOM
       RECORD    KEY
         SOK-F01 *> 倉庫コード
       FILE      STATUS    SOK-ST.

*取引先マスタ
     SELECT  HTOKMS
       ASSIGN    TO    TOKMS2
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TOK-F01 *> 相手取引先コード
       FILE STATUS     TOK-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNA00101  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   URI       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                         *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST             PIC  X(02).
     03  URI-ST             PIC  X(02).
     03  SOK-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT            PIC  X(08).
     03  DSP-GRP            PIC  X(08).
     03  DSP-PRO            PIC  X(02).
     03  DSP-FNC            PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG           PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG            PIC  9(02)  VALUE  ZERO.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-SHTDENF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-CHKLVL          PIC  9(01).
     03  SAKKBN-FLG         PIC  9(01)  VALUE  ZERO.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-ZSOKMS-INV      PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI          PIC  9(01)  VALUE  ZERO.
     03  WK-SEL-KANRNO      PIC  9(08)  VALUE  ZERO.
     03  WK-RD-KANRNO       PIC  9(08)  VALUE  ZERO.

     03  WK-JNOHNBI-ST      PIC  9(08).
     03  WK-JNOHNBI-ED      PIC  9(08).

     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).

     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).

     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).

     03  WK-BATNO.
       05  WK-BATNO-YMD     PIC  9(08).
       05  WK-BATNO-TIME    PIC  9(04).
       05  WK-BATNO-TORCD   PIC  9(08).

***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH              PIC   9(02)  VALUE  ZERO.
     03  WK-MM              PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
       05  FILLER           PIC  N(15)
       VALUE NC"_取消　_終了".
     03  PF-MSG2.
       05  FILLER           PIC  N(15)
             VALUE NC"_取消　_終了　_項目戻り".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(15)  OCCURS 2.

*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER         PIC  N(30)  VALUE
         NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER         PIC  N(30)  VALUE
         NC"バッチＮｏが未入力です。入力して下さい。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
           NC"入力されたバッチＮｏは売上伝票データに存在しません
-            "。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"このバッチＮｏの売上伝票ＤＴに小売連携対象は存在しま
-          "せん。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
         NC"入力された出荷場所は倉庫マスタに存在しません。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
         NC"納品日が未入力です。入力して下さい。".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
         NC"納品日の日付に誤りがあります。".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
         NC"入力された条件では、売上伝票データが存在しません。".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
         NC"実納品日の日付に誤りがあります。".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"実納品日は、納品日の１週間前から１ヵ月後までの日付で
-          "す。".
     03  ERR-MSG11.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
     03  ERR-MSG12.
         05  FILLER         PIC  N(30)  VALUE
         NC"正しいバッチＮｏを入力して下さい。".
     03  ERR-MSG13.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG14.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG15.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG16.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG17.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG18.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG19.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG20.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
 01  ERR-MSG-AREA-R  REDEFINES ERR-MSG-AREA.
     03  ERR-MSG-R          PIC  N(30)  OCCURS 20.
*
 01  FILE-ERR.
     03  DSP-ERR           PIC  N(20)  VALUE
         NC"画面ファイルエラー".
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  SOK-ERR           PIC  N(20)  VALUE
         NC"倉庫マスタエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".

*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
 01  PAR-BMNCD-IN           PIC  X(04).
 01  PAR-TANCD-IN           PIC  X(02).

* 出力パラメータ
*    パラＡ
 01  PAA-TANCD              PIC  X(06).
 01  PAA-ONLTEG             PIC  X(01).
 01  PAA-BATNO              PIC  X(20).
 01  PAA-BASHO              PIC  X(02).
 01  PAA-NOHNBI             PIC  9(08).
 01  PAA-JNOHNBI            PIC  9(08).
 01  PAA-TORCD              PIC  9(08).
 01  PAA-DEN1-FROM          PIC  9(09).
 01  PAA-DEN1-TO            PIC  9(09).
 01  PAA-DEN2-FROM          PIC  9(09).
 01  PAA-DEN2-TO            PIC  9(09).
 01  PAA-DEN3-FROM          PIC  9(09).
 01  PAA-DEN3-TO            PIC  9(09).
 01  PAA-DEN4-FROM          PIC  9(09).
 01  PAA-DEN4-TO            PIC  9(09).
 01  PAA-DEN5-FROM          PIC  9(09).
 01  PAA-DEN5-TO            PIC  9(09).
 01  PAA-STS                PIC  X(01).
 01  PAA-RENKEINO           PIC  X(09).
*
**************************************************************
 PROCEDURE             DIVISION
                           USING  PAR-BMNCD-IN
                                  PAR-TANCD-IN
                                  PAA-TANCD
                                  PAA-ONLTEG
                                  PAA-BATNO
                                  PAA-BASHO
                                  PAA-NOHNBI
                                  PAA-JNOHNBI
                                  PAA-TORCD
                                  PAA-DEN1-FROM
                                  PAA-DEN1-TO
                                  PAA-DEN2-FROM
                                  PAA-DEN2-TO
                                  PAA-DEN3-FROM
                                  PAA-DEN3-TO
                                  PAA-DEN4-FROM
                                  PAA-DEN4-TO
                                  PAA-DEN5-FROM
                                  PAA-DEN5-TO
                                  PAA-STS
                                  PAA-RENKEINO.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     URI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  INPUT SHTDENF.
     OPEN  INPUT ZSOKMS.
     OPEN  INPUT HTOKMS.
*ワークの初期化
     INITIALIZE  FLG-AREA.
*初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE  "1"              TO  PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     EVALUATE  PSW
*    キー入力
       WHEN  "1"  PERFORM  DSP-KEY-SEC
*    メイン入力
       WHEN  "2"  PERFORM  DSP-BODY-SEC
*    確認入力
       WHEN  "3"  PERFORM  DSP-KAKU-SEC

       WHEN  OTHER  CONTINUE
     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  キー部入力処理  ( PSW = 1 )                                 *
****************************************************************
 DSP-KEY-SEC           SECTION.
     MOVE  "DSP-KEY-SEC"    TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  KEY-CHK-SEC

*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS

*      取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.

 DSP-KEY-EXIT.
     EXIT.
****************************************************************
*  画面表示処理                                                *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.

     PERFORM  SDATE-GET-SEC.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*  エラーメッセージセット
     IF  ERR-FLG = ZERO
         MOVE  SPACE        TO  DSP-ERRMSG
     ELSE
         MOVE  ERR-MSG-R(ERR-FLG) TO  DSP-ERRMSG
         MOVE  ZERO         TO  ERR-FLG
     END-IF.
*  ガイドメッセージの設定
     EVALUATE  PSW
*    キー入力
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-PFGAID
*    メイン入力
       WHEN  "2"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
*    確認
       WHEN  "3"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID

       WHEN  OTHER
         MOVE  SPACE        TO  DSP-PFGAID
     END-EVALUATE.
*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FNA00101"       TO  DSP-FMT.
     WRITE  DSP-FNA00101.

 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*  画面の初期化
     MOVE  SPACE            TO  DSP-FNA00101.
*  ＰＧＩＤ
     MOVE  "SNA0010I"       TO  DSP-PGID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*  リバース，カーソルパーク解除
     MOVE  "M"              TO  EDIT-OPTION OF DSP-YMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TIME.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TIME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BASHO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASHO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-NOHNBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NOHNBI.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JNOHBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JNOHBI.

 INT-DSP-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.
     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE   PSW
*    処理区分
       WHEN  "1"
         MOVE  "KEY"        TO  DSP-GRP

*    明細項目（オンライン）
       WHEN  "2"
         MOVE  "MAIN"       TO  DSP-GRP

*    確認
       WHEN  "3"
         MOVE  "KAKU"       TO  DSP-GRP
     END-EVALUATE.

     MOVE  "FNA00101"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
*    MOVE  ZERO             TO   ERR-FLG.
     MOVE  SPACE            TO   DSP-PRO.

 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  キー部チェック                                              *
****************************************************************
 KEY-CHK-SEC          SECTION.
     MOVE  "KEY-CHK-SEC"    TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-YMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TIME.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TIME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORCD.

* バッチＮ-
     IF      (DSP-YMD   = SPACE OR ZERO)
         AND (DSP-TIME  = SPACE OR ZERO)
         AND (DSP-TORCD = SPACE OR ZERO)
         IF  ERR-FLG = ZERO
             MOVE  2     TO  ERR-FLG
         END-IF
         MOVE  "C"           TO  EDIT-CURSOR OF DSP-YMD
         MOVE  "R"           TO  EDIT-OPTION OF DSP-YMD
         MOVE  "R"           TO  EDIT-OPTION OF DSP-TIME
         MOVE  "R"           TO  EDIT-OPTION OF DSP-TORCD
         MOVE  SPACE         TO  DSP-YMD   (1:8)
         MOVE  SPACE         TO  DSP-TIME  (1:4)
         MOVE  SPACE         TO  DSP-TORCD (1:8)
         GO TO  KEY-CHK-EXIT
     END-IF.


     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-YMD          TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  12       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-YMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-YMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TIME
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TORCD
         GO TO  KEY-CHK-EXIT
     END-IF.

     MOVE  DSP-TORCD        TO  TOK-F01.
     PERFORM  RD-HTOKMS-SEC.
     IF  FG-HTOKMS-INV = ZERO
         MOVE  TOK-F02      TO  DSP-TORNM
     ELSE
         MOVE  ALL NC"＊"   TO  DSP-TORNM
         IF  ERR-FLG = ZERO
             MOVE  12       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-YMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-YMD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TIME
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TORCD
         GO TO  KEY-CHK-EXIT
     END-IF.

     MOVE  LOW-VALUE     TO  FG-SHTDENF-END.

     INITIALIZE  URI-REC.
     MOVE  DSP-YMD       TO  URI-F46. *> 受信日付
     MOVE  DSP-TIME      TO  URI-F47. *> 受信時間
     MOVE  DSP-TORCD     TO  URI-F01  *> 取引先ＣＤ
     MOVE  LOW-VALUE     TO  FG-SHTDENF-END.
     MOVE  ZERO          TO  FG-CHKLVL.

     PERFORM  RD-SHTDENF-SEC.
     IF  FG-SHTDENF-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  3     TO  ERR-FLG
         END-IF
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-YMD
         MOVE  "R"       TO  EDIT-OPTION OF DSP-YMD
         MOVE  "R"       TO  EDIT-OPTION OF DSP-TIME
         MOVE  "R"       TO  EDIT-OPTION OF DSP-TORCD
         GO TO  KEY-CHK-EXIT
     END-IF.

     MOVE  LOW-VALUE     TO  FG-SHTDENF-END.

     INITIALIZE  URI-REC.
     MOVE  DSP-YMD       TO  URI-F46. *> 受信日付
     MOVE  DSP-TIME      TO  URI-F47. *> 受信時間
     MOVE  DSP-TORCD     TO  URI-F01  *> 取引先ＣＤ
     MOVE  LOW-VALUE     TO  FG-SHTDENF-END.
     MOVE  1             TO  FG-CHKLVL.

     PERFORM  RD-SHTDENF-SEC.
     IF  FG-SHTDENF-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  4     TO  ERR-FLG
         END-IF
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-YMD
         MOVE  "R"       TO  EDIT-OPTION OF DSP-YMD
         MOVE  "R"       TO  EDIT-OPTION OF DSP-TIME
         MOVE  "R"       TO  EDIT-OPTION OF DSP-TORCD
         GO TO  KEY-CHK-EXIT
     END-IF.

*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  KEY-CHK-EXIT
     END-IF.

     MOVE  2             TO  PSW.

 KEY-CHK-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.

 RD-HTOKMS-EXIT.
     EXIT.

****************************************************************
*    売上伝票ファイル読込み　                                  *
****************************************************************
 RD-SHTDENF-SEC          SECTION.
*
     IF  FG-SHTDENF-END = LOW-VALUE
         START  SHTDENF  KEY >=  URI-F461
                                 URI-F462
                                 URI-F463
                                 URI-F471
                                 URI-F472
                                 URI-F01
                                 URI-F48
                                 URI-F02
                                 URI-F04
                                 URI-F051
                                 URI-F07
                                 URI-F112
                                 URI-F03
           INVALID KEY
              MOVE  "END"   TO  FG-SHTDENF-END
              GO TO  RD-SHTDENF-EXIT
         END-START
*
         MOVE  SPACE        TO  FG-SHTDENF-END
*
     END-IF.
*
     READ  SHTDENF
       AT  END
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-READ.
*
*     DISPLAY "F462="  URI-F462
*             ",F463=" URI-F463
*             ",F471=" URI-F471
*             ",F472=" URI-F472
*             ",F01="  URI-F01
*             ",F48="  URI-F48
*             ",F02="  URI-F02
*             ",F04="  URI-F04
*             ",F051=" URI-F051
*             ",F07="  URI-F07
*             ",F112=" URI-F112
*             ",F03="  URI-F03 "*"  UPON CONS.
* 受信日
     IF  URI-F46 > DSP-YMD
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
* 受信時間
     IF  URI-F47 > DSP-TIME
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
* 取引先コード
     IF  URI-F01 > DSP-TORCD
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*----------------------------------------------
* 未指定タイミング****
     EVALUATE  TRUE
       WHEN  FG-CHKLVL = ZERO
         CONTINUE
*
*バッチ_指定タイミング****
       WHEN  FG-CHKLVL = 1
*   *> 小売連携区分
*↓2012/07/12
* ↓2016/10/20 ST コメントアウト位置を変更
         IF  URI-F32 NOT = "1"  OR  URI-F33 NOT = "         "
             GO TO  RD-SHTDENF-SEC
         END-IF
*********IF  URI-F33 NOT = "         "
*********    GO TO  RD-SHTDENF-SEC
*********END-IF
* ↑2016/10/20 ED コメントアウト位置を変更
*↑2012/07/12
*
*場所・納品日指定タイミング****
       WHEN  FG-CHKLVL = 2
*   *> 場所
*↓2012/07/12
*********IF       DSP-BASHO NOT = SPACE
*********    AND  URI-F48 > DSP-BASHO
*********    MOVE  "END"    TO  FG-SHTDENF-END
*********    GO TO  RD-SHTDENF-EXIT
*********END-IF
*場所指定時
         IF  ( DSP-BASHO NOT = SPACE )
             IF   ( URI-F48 > DSP-BASHO )  OR
                  ( SOK-F12 NOT = "1" )
                   MOVE  "END"    TO  FG-SHTDENF-END
                   GO TO  RD-SHTDENF-EXIT
             END-IF
         END-IF
*場所無指定時
         IF  ( DSP-BASHO     = SPACE )
             MOVE  URI-F48   TO  SOK-F01
             PERFORM RD-ZSOKMS-SEC
             IF    FG-ZSOKMS-INV = 1
                   GO TO  RD-SHTDENF-SEC
             END-IF
             IF    SOK-F12   NOT = "1"
                   GO TO  RD-SHTDENF-SEC
             END-IF
         END-IF
*↑2012/07/12
*
*   *> 納品日
         IF  URI-F112 NOT = DSP-NOHNBI
             GO TO  RD-SHTDENF-SEC
         END-IF
*
*   *> 小売連携区分
*↓2012/07/12
* * ↓2016/10/20 ST コメントアウト位置を変更
         IF  URI-F32 NOT = "1"  OR  URI-F33 NOT = "         "
             GO TO  RD-SHTDENF-SEC
         END-IF
*********IF  URI-F33 NOT = "         "
*********    GO TO  RD-SHTDENF-SEC
*********END-IF
* * ↑2016/10/20 ED コメントアウト位置を変更
*↑2012/07/12
*↓2012/09/21
*   *> 小売連携区分
         IF  URI-F15  <=  ZERO
             GO TO  RD-SHTDENF-SEC
         END-IF
*↑2012/09/21
*
     END-EVALUATE.
*
 RD-SHTDENF-EXIT.
     EXIT.
*
****************************************************************
*  明細項目（オンライン）入力  ( PSW = 2 )                     *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE  "DSP-BODY-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  BODY-CHK-SEC

*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS

*    項目戻し
       WHEN  "F006"
         MOVE  "1"          TO  PSW

*    取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-BODY-SEC

     END-EVALUATE.

 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE "BODY-CHK-SEC"    TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-BASHO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASHO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-NOHNBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NOHNBI.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JNOHBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JNOHBI.

* 場所
     MOVE  SPACE            TO  DSP-BASHNM.
     IF  DSP-BASHO = SPACE
         GO TO  BODY-CHK-BASHO-EXIT
     END-IF.

     MOVE  DSP-BASHO        TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         GO TO  BODY-CHK-EXIT
     END-IF.

     MOVE  SOK-F02          TO  DSP-BASHNM.

 BODY-CHK-BASHO-EXIT.
* 納品日
     IF DSP-NOHNBI = SPACE OR ZERO
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-NOHNBI
        MOVE  "R"           TO  EDIT-OPTION OF DSP-NOHNBI
        GO TO  BODY-CHK-NOHNBI-EXIT
     END-IF.

     MOVE  "2"              TO  LINK-IN-KBN.
     MOVE  DSP-NOHNBI       TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  7        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-NOHNBI
         MOVE  "R"          TO  EDIT-OPTION OF DSP-NOHNBI
         GO TO  BODY-CHK-NOHNBI-EXIT
     END-IF.

     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK-NOHNBI-EXIT
     END-IF.

     MOVE  LOW-VALUE     TO  FG-SHTDENF-END.

     INITIALIZE  URI-REC.
     MOVE  DSP-YMD       TO  URI-F46. *> 受信日付
     MOVE  DSP-TIME      TO  URI-F47. *> 受信時間
     MOVE  DSP-TORCD     TO  URI-F01  *> 取引先ＣＤ
     MOVE  DSP-BASHO     TO  URI-F48  *> 振分倉庫
     MOVE  LOW-VALUE     TO  FG-SHTDENF-END.
     MOVE  2             TO  FG-CHKLVL.

     PERFORM  RD-SHTDENF-SEC.
     IF  FG-SHTDENF-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  8     TO  ERR-FLG
         END-IF
         MOVE  "C"       TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"       TO  EDIT-OPTION OF DSP-BASHO
         MOVE  "R"       TO  EDIT-OPTION OF DSP-NOHNBI
         GO TO  BODY-CHK-NOHNBI-EXIT
     END-IF.

 BODY-CHK-NOHNBI-EXIT.
* 実納品日
     IF  DSP-JNOHBI = SPACE OR ZERO
         IF  ERR-FLG NOT = ZERO
             GO TO  BODY-CHK-JNOHNBI-EXIT
         END-IF
      *> 未指定時、納品日を実納品日とする。
         MOVE  DSP-NOHNBI   TO  DSP-JNOHBI
     END-IF.

     MOVE  "2"             TO  LINK-IN-KBN.
     MOVE  DSP-JNOHBI      TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-JNOHBI
         MOVE  "R"          TO  EDIT-OPTION OF DSP-JNOHBI
         GO TO  BODY-CHK-JNOHNBI-EXIT
     END-IF.

     PERFORM  JNOHNBI-HANI-KEISAN-SEC.
     DISPLAY "#" NC"納品日　" "=" DSP-JNOHBI    " #" UPON CONS.
     DISPLAY "#" NC"範囲開始" "=" WK-JNOHNBI-ST " #" UPON CONS.
     DISPLAY "#" NC"範囲終了" "=" WK-JNOHNBI-ED " #" UPON CONS.
     IF     DSP-JNOHBI < WK-JNOHNBI-ST
         OR DSP-JNOHBI > WK-JNOHNBI-ED
         IF  ERR-FLG = ZERO
             MOVE  10       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-JNOHBI
         MOVE  "R"          TO  EDIT-OPTION OF DSP-JNOHBI
         GO TO  BODY-CHK-JNOHNBI-EXIT
     END-IF.

 BODY-CHK-JNOHNBI-EXIT.

* チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK-EXIT
     END-IF.

     MOVE  "3"              TO  PSW.
     MOVE  11               TO  ERR-FLG.

 BODY-CHK-EXIT.
     EXIT.

****************************************************************
*    倉庫マスタ検索                                            *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     READ  ZSOKMS
       INVALID
         MOVE  1                 TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-ZSOKMS-INV
     END-READ.

 RD-ZSOKMS-EXIT.
     EXIT.

****************************************************************
*  実納品日入力可期間計算                                      *
****************************************************************
 JNOHNBI-HANI-KEISAN-SEC    SECTION.
*    実納品日の入力可能期間は
*    「納品日を基準にし過去１週間～将来１ヵ月間」

     MOVE  ZERO             TO  WK-JNOHNBI-ST.
     MOVE  ZERO             TO  WK-JNOHNBI-ED.
*  期間開始日
     MOVE  DSP-NOHNBI  TO  WK-YMD.
     PERFORM  HZK-CVT-SEC.
     COMPUTE  WK-CMPYMD-D = WK-CMPYMD-D - 7.
     IF  WK-CMPYMD-D < 1
         COMPUTE  WK-CMPYMD-M = WK-CMPYMD-M - 1
         IF WK-CMPYMD-M < 1
            MOVE  12        TO  WK-CMPYMD-M
            COMPUTE  WK-CMPYMD-Y = WK-CMPYMD-Y - 1
         END-IF
         PERFORM  MATUBI-KEISAN-SEC
         COMPUTE  WK-CMPYMD-D = WK-CMPYMD-D + WK-MATUBI
     END-IF.

     PERFORM  HZK-CVT2-SEC.
     MOVE  WK-YMD           TO  WK-JNOHNBI-ST.

*  期間終了日
     MOVE  DSP-NOHNBI       TO  WK-YMD.
     PERFORM  HZK-CVT-SEC.
     COMPUTE  WK-CMPYMD-M = WK-CMPYMD-M + 1.
     IF  WK-CMPYMD-M > 12
         COMPUTE  WK-CMPYMD-M = WK-CMPYMD-M - 12
         COMPUTE  WK-CMPYMD-Y = WK-CMPYMD-Y + 1
     END-IF.
     PERFORM  HZK-CVT2-SEC.
     MOVE  WK-YMD           TO  WK-JNOHNBI-ED.

 JNOHNBI-HANI-KEISAN-EXIT.
     EXIT.

****************************************************************
*  日付変換                                                    *
****************************************************************
 HZK-CVT-SEC    SECTION.
     MOVE  WK-YMD-Y         TO  WK-CMPYMD-Y.
     MOVE  WK-YMD-M         TO  WK-CMPYMD-M.
     MOVE  WK-YMD-D         TO  WK-CMPYMD-D.
 HZK-CVT-EXIT.
     EXIT.

****************************************************************
*  月末日計算                                                  *
****************************************************************
 MATUBI-KEISAN-SEC    SECTION.
     MOVE  ZERO             TO  WK-MATUBI.

     EVALUATE  TRUE
       WHEN  WK-CMPYMD-M = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
*2013/08/19 NAV ST 末日の転送を変更
*********MOVE 31            TO  WK-CMPYMD-D
         MOVE 31            TO  WK-MATUBI
*2013/08/19 NAV ED 末日の転送を変更

       WHEN  WK-CMPYMD-M = 4 OR 6 OR 9 OR 11
*2013/08/19 NAV ST 末日の転送を変更
*********MOVE 30            TO  WK-CMPYMD-D
         MOVE 30            TO  WK-MATUBI
*2013/08/19 NAV ED 末日の転送を変更

       WHEN  OTHER
         PERFORM  URUUTUKI-KEISAN-SEC

     END-EVALUATE.

 MATUBI-KEISAN-EXIT.
     EXIT.

****************************************************************
*  閏月末日計算                                                *
****************************************************************
 URUUTUKI-KEISAN-SEC    SECTION.

*2013/08/19 NAV ST 末日の転送を変更
*****MOVE  28               TO  WK-CMPYMD-D.
     MOVE  28               TO  WK-MATUBI.
*2013/08/19 NAV ED 末日の転送を変更

     DIVIDE  4   INTO  WK-CMPYMD-Y
         GIVING    WK-SYO
         REMAINDER WK-AMARI.

     IF  WK-AMARI NOT = ZERO *> 平年
         GO TO  URUUTUKI-KEISAN-EXIT
     END-IF.

     DIVIDE  100   INTO  WK-CMPYMD-Y
         GIVING    WK-SYO
         REMAINDER WK-AMARI.

     IF  WK-AMARI NOT = ZERO *> 閏年
*2013/08/19 NAV ST 末日の転送を変更
*********COMPUTE  WK-CMPYMD-D = WK-CMPYMD-D + 1
         COMPUTE  WK-MATUBI   = WK-MATUBI   + 1
*2013/08/19 NAV ED 末日の転送を変更
         GO TO  URUUTUKI-KEISAN-EXIT
     END-IF.

     DIVIDE  400   INTO  WK-CMPYMD-Y
         GIVING    WK-SYO
         REMAINDER WK-AMARI.

     IF  WK-AMARI NOT = ZERO *> 平年
         CONTINUE
     ELSE                *> 閏年
*2013/08/19 NAV ST 末日の転送を変更
*********COMPUTE  WK-CMPYMD-D = WK-CMPYMD-D + 1
         COMPUTE  WK-MATUBI   = WK-MATUBI   + 1
*2013/08/19 NAV ED 末日の転送を変更
     END-IF.

 URUUTUKI-KEISAN-EXIT.
     EXIT.

****************************************************************
*  日付変換                                                    *
****************************************************************
 HZK-CVT2-SEC    SECTION.
     MOVE  WK-CMPYMD-Y      TO  WK-YMD-Y.
     MOVE  WK-CMPYMD-M      TO  WK-YMD-M.
     MOVE  WK-CMPYMD-D      TO  WK-YMD-D.
 HZK-CVT2-EXIT.
     EXIT.

****************************************************************
*  確認処理入力 （ PSW = 4 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  PARA-OT-SEC
         MOVE  "END"        TO  END-FLG
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*    項目戻し
       WHEN  "F006"
         MOVE  "2"          TO  PSW
*    取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-KAKU-SEC

     END-EVALUATE.

 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.

* 部門・担当者ＣＤ
     MOVE  SPACE            TO  PAA-TANCD.
     STRING
       PAR-BMNCD-IN  PAR-TANCD-IN
       DELIMITED BY SIZE  INTO  PAA-TANCD.

* オンライン/手書種別
     MOVE  "2"              TO  PAA-ONLTEG.

* バッチＮｏ
     MOVE  DSP-YMD          TO  WK-BATNO-YMD.
     MOVE  DSP-TIME         TO  WK-BATNO-TIME.
     MOVE  DSP-TORCD        TO  WK-BATNO-TORCD.
     MOVE  WK-BATNO         TO  PAA-BATNO.

* 出荷場所
     MOVE  DSP-BASHO        TO  PAA-BASHO.

* 納品日
     MOVE  DSP-NOHNBI       TO  PAA-NOHNBI.

* 実納品日
     MOVE  DSP-JNOHBI       TO  PAA-JNOHNBI.

* 取引先ＣＤ
     MOVE  DSP-TORCD        TO  PAA-TORCD.

* 伝票Ｎｏ１（ＦＲＯＭ）
     MOVE  ZERO             TO  PAA-DEN1-FROM.
* 伝票Ｎｏ１（ＴＯ）
     MOVE  ALL "9"          TO  PAA-DEN1-TO.

* 伝票Ｎｏ２（ＦＲＯＭ）
     MOVE  ZERO             TO  PAA-DEN2-FROM.
* 伝票Ｎｏ２（ＴＯ）
     MOVE  ALL "9"          TO  PAA-DEN2-TO.

* 伝票Ｎｏ３（ＦＲＯＭ）
     MOVE  ZERO             TO  PAA-DEN3-FROM.
* 伝票Ｎｏ３（ＴＯ）
     MOVE  ALL "9"          TO  PAA-DEN3-TO.

* 伝票Ｎｏ４（ＦＲＯＭ）
     MOVE  ZERO             TO  PAA-DEN4-FROM.
* 伝票Ｎｏ４（ＴＯ）
     MOVE  ALL "9"          TO  PAA-DEN4-TO.

* 伝票Ｎｏ５（ＦＲＯＭ）
     MOVE  ZERO             TO  PAA-DEN5-FROM.
* 伝票Ｎｏ５（ＴＯ）
     MOVE  ALL "9"          TO  PAA-DEN5-TO.

* 状態
     MOVE  SPACE            TO  PAA-STS.

* 連携Ｎｏ
     MOVE  ZERO             TO  PAA-RENKEINO.

 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  SHTDENF.
     CLOSE  ZSOKMS.
     CLOSE  HTOKMS.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SNA0010I   END PROGRAM  >>******************

```
