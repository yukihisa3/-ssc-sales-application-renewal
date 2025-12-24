# SNA0610I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SNA0610I.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：先行受注発注データ抽出指示（手書き）
*    作成日／更新日　：2011/10/28
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      苗業務システムへ送信する先行受注（手書き分）
*      発注データを、売上伝票ファイルより抽出するため
*      の条件指定。
*      チェックＯＫなら条件をパラメータに出力する。
*    変更日／更新日　：2012/07/13
*    変更内容　　　　：
*      売上伝票データの先行受注区分はノーチェックにし、
*      倉庫マスタの先行受注区分をチェックする。
*    作成日／更新日　：2012/09/20,21
*    作成者／更新者　：NAV TAKAHAHSI & INOUE
*    修正概要　　　　　　：
*    　売上伝票データのＬＦを変更し速度ＵＰ。　
*    作成日／更新日　：2013/01/25
*    作成者／更新者　：NAV TAKAHAHSI
*    修正概要　　管理番号を追加。
*　　　　　　　　データ存在チェック追加、パラメタ追加（出力）
*    変更日／更新日　：2016/10/20
*    変更内容　　　　：
*      先行受注商品のみを対象とする
*      ２０１２／０７／１３対応分の戻し作業
*    変更日／更新日　：2020/05/01 T.TAKAHASHI
*    変更内容　　　　：Ｄ３６５連携対応
*    変更日／更新日　：2020/12/14 NAV T.TAKAHASHI
*    変更内容　　　　：Ｄ３６５連携対応（ＯＲＤ区分変更）
*    変更日／更新日　：2021/01/05 NAV T.TAKAHASHI
*    変更内容　　　　：Ｄ３６５連携対応（返品データ対応）
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0610I.
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
       ASSIGN    TO        SHTDENLF
       ORGANIZATION        INDEXED
       ACCESS    MODE      SEQUENTIAL
       RECORD    KEY
         URI-F277 *> 売上データ作成
         URI-F274 *> オンライン
         URI-F01  *> 取引先コード
         URI-F08  *> 出荷場所
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
*条件ファイル
     SELECT  HJYOKEN
       ASSIGN    TO    JYOKEN1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         JYO-F01
         JYO-F02
       FILE STATUS     JYO-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FNA06101  OF   XMDLIB
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
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST             PIC  X(02).
     03  URI-ST             PIC  X(02).
     03  SOK-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  JYO-ST             PIC  X(02).
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
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-ZSOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-DENNO-ARI       PIC  9(01).
     03  FG-NOHNBI-CHK      PIC  9(01).
     03  FG-NOHNBI-SENTO    PIC  9(01).
     03  FG-DENALL-SPACE    PIC  9(01).
     03  FG-HJYOKEN-INV     PIC  9(01)  VALUE  ZERO.
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

     03  IX                 PIC  9(04).
     03  IX-DENNO           PIC  9(02).

     03  FG-DENNO-ARI-MS-G.
       05  FG-DENNO-ARI-MS-G2 OCCURS 5.
         07  FG-DENNO-ARI-MS     PIC  9(01).
         07  FG-DENNO-ARI-KOURI-RENKEI
                                 PIC  9(01).
     03  WK-NOHNBI               PIC  9(08).
     03  WK-IN-GYOSU        PIC  9(03).
     03  WK-INMAX-GYOSU     PIC  9(03).
*# 2020/05/01 NAV ST Ｄ３６５連携対応
 01  RENKBN                 PIC  X(01)  VALUE  SPACE.
*# 2020/05/01 NAV ED Ｄ３６５連携対応
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

*A---B-------2---------3---------4---------5---------6---------7E*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER         PIC  N(30)  VALUE
         NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER         PIC  N(30)  VALUE
         NC"出荷場所が未入力です。入力してください。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
         NC"入力された出荷場所は倉庫マスタに存在しません。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
         NC"入力された取引先は取引先マスタに存在しません。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
         NC"実納品日の日付に誤りがあります。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"実納品日は、納品日の１週間前から１ヵ月後までの日付で
-          "す。".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
         NC"伝票Ｎｏ（開始・終了）の大小が逆です。".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"入力された伝票Ｎｏ範囲内の売上伝票データは存在しませ
-          "ん。".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
           NC"入力された伝票Ｎｏ範囲内に複数の納品日が存在します
-            "。".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
         NC"必ず１行目から指定して下さい。".
     03  ERR-MSG11.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"このバッチＮｏの売上伝票データに先行受注対象は存在し
-          "ません。".
     03  ERR-MSG12.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
     03  ERR-MSG13.
         05  FILLER         PIC  N(30)  VALUE
         NC"伝票Ｎｏ範囲を指定して下さい。".
     03  ERR-MSG14.
         05  FILLER         PIC  N(30)  VALUE
         NC"行を詰めて入力して下さい。".
     03  ERR-MSG15.
         05  FILLER         PIC  N(30)  VALUE
*↓2012/07/13
         NC"連携対象の出荷場所ではありません。".
*↑2012/07/13
     03  ERR-MSG16.
         05  FILLER         PIC  N(30)  VALUE
*2013/01/25 NAV ST
         NC"ナフコの場合、管理番号を必ず入力して下さい。".
*2013/01/25 NAV ED
     03  ERR-MSG17.
         05  FILLER         PIC  N(30)  VALUE
         NC"オーダー区分がマスタに存在しません。".
     03  ERR-MSG18.
         05  FILLER         PIC  N(30)  VALUE
         NC"オーダー区分を入力して下さい。".
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
     03  JYO-ERR           PIC  N(20)  VALUE
         NC"条件ファイルエラー".

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
*    パラメータＡ
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
*2013/01/25 NAV ST
 01  PAA-KANRINO            PIC  9(08).
*2013/01/25 NAV ED
*2020/05/01 NAV ST
 01  PAA-ORDER-KBN          PIC  X(01).
*2020/05/01 NAV ED
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
*2013/01/25 NAV ST
**********************************PAA-RENKEINO.
                                  PAA-RENKEINO
*# 2020/05/01 NAV ST Ｄ３６５連携対応
**********************************PAA-KANRINO.
                                  PAA-KANRINO
                                  PAA-ORDER-KBN.
*# 2020/05/01 NAV ED Ｄ３６５連携対応
*2013/01/25 NAV ED
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
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.
*
     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
*抽出データ判定
*# 2020/04/30 NAV ST Ｄ３６５連携対応
*****IF  PAA-ORDER-KBN  =  "1"
*        DISPLAY NC"＃直送分データ抽出＃" UPON CONS
*        MOVE  "2"               TO  RENKBN
*    ELSE
*        IF  PAA-ORDER-KBN  =  "2"
*            DISPLAY NC"＃セット組分データ抽出＃" UPON CONS
*            MOVE "3"            TO  RENKBN
*        ELSE
*            DISPLAY NC"＃抽出対象外！！＃" UPON CONS
*            MOVE   4000    TO   PROGRAM-STATUS
*            STOP  RUN
*        END-IF
*****END-IF.
*ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  INPUT SHTDENF.
     OPEN  INPUT ZSOKMS.
     OPEN  INPUT HTOKMS.
     OPEN  INPUT HJYOKEN.
*ワークの初期化
     INITIALIZE  FLG-AREA.
*初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
     MOVE  SPACE            TO  PSW.
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
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
*
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
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*  リバース，カーソルパーク解除、クリア
*
* 2011/12/06,S S.I/NAV
*   クリアはどれでも先頭に戻り全項目クリアに変更
**     EVALUATE  PSW
***    出荷場所
**       WHEN  "1"
**         MOVE  SPACE        TO  DSP-BASHO
**         MOVE  SPACE        TO  DSP-BASHNM
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-BASHO
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-BASHO
**
**         MOVE  SPACE        TO  DSP-TORCD (1:8)
**         MOVE  SPACE        TO  DSP-TORNM
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD
**
**         MOVE  SPACE        TO  DSP-JNOHBI (1:8)
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-JNOHBI
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-JNOHBI
**
**         PERFORM  VARYING IX  FROM 1 BY 1
**                  UNTIL   IX > 5
**           MOVE  SPACE      TO  DSP-SDENNO (IX) (1:9)
**           MOVE  SPACE      TO  DSP-EDENNO (IX) (1:9)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-SDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SDENNO (IX)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-EDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-EDENNO (IX)
**
**         END-PERFORM
**
***    取引先
**       WHEN  "2"
**         MOVE  SPACE        TO  DSP-TORCD (1:8)
**         MOVE  SPACE        TO  DSP-TORNM
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD
**
**         MOVE  SPACE        TO  DSP-JNOHBI (1:8)
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-JNOHBI
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-JNOHBI
**
**         PERFORM  VARYING IX  FROM 1 BY 1
**                  UNTIL   IX > 5
**           MOVE  SPACE      TO  DSP-SDENNO (IX) (1:9)
**           MOVE  SPACE      TO  DSP-EDENNO (IX) (1:9)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-SDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SDENNO (IX)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-EDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-EDENNO (IX)
**
**         END-PERFORM
**
***    実納品日
**       WHEN  "3"
**         MOVE  SPACE        TO  DSP-JNOHBI (1:8)
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-JNOHBI
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-JNOHBI
**
**         PERFORM  VARYING IX  FROM 1 BY 1
**                  UNTIL   IX > 5
**           MOVE  SPACE      TO  DSP-SDENNO (IX) (1:9)
**           MOVE  SPACE      TO  DSP-EDENNO (IX) (1:9)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-SDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SDENNO (IX)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-EDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-EDENNO (IX)
**
**         END-PERFORM
**
***    伝票番号
**       WHEN  "4"
**         MOVE  SPACE        TO  DSP-BASHO
**         MOVE  SPACE        TO  DSP-BASHNM
**         MOVE  SPACE        TO  DSP-TORCD  (1:8)
**         MOVE  SPACE        TO  DSP-TORNM
**         MOVE  SPACE        TO  DSP-JNOHBI (1:8)
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-BASHO
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-BASHO
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-JNOHBI
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-JNOHBI
**
**         PERFORM  VARYING IX  FROM 1 BY 1
**                  UNTIL   IX > 5
**           MOVE  SPACE      TO  DSP-SDENNO (IX) (1:9)
**           MOVE  SPACE      TO  DSP-EDENNO (IX) (1:9)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-SDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SDENNO (IX)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-EDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-EDENNO (IX)
**
**         END-PERFORM
***    確認
**       WHEN  "5"
**         MOVE  SPACE        TO  DSP-BASHO
**         MOVE  SPACE        TO  DSP-BASHNM
**         MOVE  SPACE        TO  DSP-TORCD  (1:8)
**         MOVE  SPACE        TO  DSP-TORNM
**         MOVE  SPACE        TO  DSP-JNOHBI (1:8)
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-BASHO
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-BASHO
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD
**         MOVE  "M"          TO  EDIT-OPTION OF DSP-JNOHBI
**         MOVE  SPACE        TO  EDIT-CURSOR OF DSP-JNOHBI
**
**         PERFORM  VARYING IX  FROM 1 BY 1
**                  UNTIL   IX > 5
**           MOVE  SPACE      TO  DSP-SDENNO (IX) (1:9)
**           MOVE  SPACE      TO  DSP-EDENNO (IX) (1:9)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-SDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SDENNO (IX)
**           MOVE  "M"        TO  EDIT-OPTION OF DSP-EDENNO (IX)
**           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-EDENNO (IX)
**
**         END-PERFORM
**
***    画面の初期化
**       WHEN  OTHER
**         MOVE  SPACE        TO  DSP-FNA06101
**
**     END-EVALUATE.
* 2011/12/06,E S.I/NAV
*
* 2011/12/06,S S.I/NAV
*  クリアはどれでも先頭に戻り全項目クリアに変更
     MOVE  SPACE            TO  DSP-FNA06101.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BASHO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASHO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-ORDKBN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-ORDKBN.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JNOHBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JNOHBI.
*2013/01/25 NAV ST
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KANRNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANRNO.
*2013/01/25 NAV ED
*
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 5
       MOVE  "M"            TO  EDIT-OPTION OF DSP-SDENNO (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-SDENNO (IX)
       MOVE  "M"            TO  EDIT-OPTION OF DSP-EDENNO (IX)
       MOVE  SPACE          TO  EDIT-CURSOR OF DSP-EDENNO (IX)

     END-PERFORM.
* 2011/12/06,E S.I/NAV
*
*  ＰＧＩＤ
     MOVE  "SNA0610I"       TO  DSP-PGID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*
     EVALUATE  PSW
*    出荷場所
       WHEN  "1"  PERFORM  DSP-BASHO-SEC
*    取引先
       WHEN  "2"  PERFORM  DSP-TORCD-SEC
*    実納品日
       WHEN  "3"  PERFORM  DSP-JNOHBI-SEC
*    伝票番号
       WHEN  "4"  PERFORM  DSP-DENNO-SEC
*    確認
       WHEN  "5"  PERFORM  DSP-KAKU-SEC
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*  出荷場所入力処理  ( PSW = 1 )                               *
****************************************************************
 DSP-BASHO-SEC           SECTION.
     MOVE  "DSP-BASHO-SEC"  TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  BASHO-CHK-SEC
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
*
 DSP-BASHO-EXIT.
     EXIT.
****************************************************************
*  画面表示処理                                                *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  "DSP-WRITE-SEC"  TO  S-NAME.
*
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
*    出荷場所
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-PFGAID
*    取引先
       WHEN  "2"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
*    実納品日
       WHEN  "3"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
*    伝票番号
       WHEN  "4"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
*    確認
       WHEN  "5"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE        TO  DSP-PFGAID
     END-EVALUATE.
*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FNA06101"       TO  DSP-FMT.
     WRITE  DSP-FNA06101.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.
*
     MOVE  "NE"             TO  DSP-PRO.
*
     EVALUATE  PSW
*    出荷場所
       WHEN  "1"
*********MOVE  "BASHO"      TO  DSP-GRP
         MOVE  "KEY1"       TO  DSP-GRP
*    取引先
       WHEN  "2"
         MOVE  "TORCD"      TO  DSP-GRP
*    実納品日
       WHEN  "3"
         MOVE  "JNOHBI"     TO  DSP-GRP
*    伝票番号
       WHEN  "4"
         MOVE  "MAIN"       TO  DSP-GRP
*    確認
       WHEN  "5"
         MOVE  "KAKU"       TO  DSP-GRP
     END-EVALUATE.
*
     MOVE  "FNA06101"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
*    MOVE  ZERO             TO   ERR-FLG.
     MOVE  SPACE            TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  出荷場所チェック                                            *
****************************************************************
 BASHO-CHK-SEC          SECTION.
     MOVE  "BASHO-CHK-SEC"  TO  S-NAME.
*
     MOVE  "M"              TO  EDIT-OPTION OF DSP-ORDKBN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-ORDKBN.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-BASHO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASHO.
*
* オーダー区分
     IF  DSP-ORDKBN = SPACE
         IF  ERR-FLG = ZERO
             MOVE  18       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-ORDKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-ORDKBN
         GO TO  BASHO-CHK-EXIT
     END-IF.

     MOVE  27               TO  JYO-F01.
     MOVE  DSP-ORDKBN       TO  JYO-F02.
     PERFORM  RD-HJYOKEN-SEC.
     IF  FG-HJYOKEN-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  17       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-ORDKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-ORDKBN
         MOVE  SPACE        TO  DSP-ORDNM
         GO TO  BASHO-CHK-EXIT
     END-IF.
*オーダー区分名
     MOVE  JYO-F03          TO  DSP-ORDNM.
     MOVE  DSP-ORDKBN       TO  RENKBN.
* 場所
     MOVE  SPACE            TO  DSP-BASHNM.
     IF  DSP-BASHO = SPACE
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         GO TO  BASHO-CHK-EXIT
     END-IF.
*
     MOVE  DSP-BASHO        TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         GO TO  BASHO-CHK-EXIT
     END-IF.
     MOVE  SOK-F02          TO  DSP-BASHNM.
*
*↓2012/07/13 連携対象倉庫チェック
     IF  SOK-F12 NOT = "1"
         IF  ERR-FLG = ZERO
             MOVE  15       TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-BASHO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-BASHO
         GO TO  BASHO-CHK-EXIT
     END-IF.
*↑2012/07/13
*
*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  BASHO-CHK-EXIT
     END-IF.
*
     MOVE  2             TO  PSW.
*
 BASHO-CHK-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索                                            *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     MOVE  "RD-ZSOKMS-SEC"  TO  S-NAME.
*
     READ  ZSOKMS
       INVALID
         MOVE  1                 TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-ZSOKMS-INV
     END-READ.
*
 RD-ZSOKMS-EXIT.
     EXIT.
****************************************************************
*    条件ファイル検索                                          *
****************************************************************
 RD-HJYOKEN-SEC         SECTION.
     READ  HJYOKEN
       INVALID
         MOVE  1                 TO  FG-HJYOKEN-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HJYOKEN-INV
     END-READ.

 RD-HJYOKEN-EXIT.
     EXIT.
****************************************************************
*  取引先入力処理  ( PSW = 2 )                                 *
****************************************************************
 DSP-TORCD-SEC           SECTION.
     MOVE  "DSP-BASHO-SEC"  TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  TORCD-CHK-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*      取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW
*      項目戻し
       WHEN  "F006"
         MOVE  "1"          TO  PSW
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.
*
 DSP-TORCD-EXIT.
     EXIT.
****************************************************************
*  取引先チェック                                              *
****************************************************************
 TORCD-CHK-SEC          SECTION.
     MOVE  "TORCD-CHK-SEC"  TO  S-NAME.
*
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORCD.
*
* 取引先
     MOVE  SPACE            TO  DSP-TORNM.
     MOVE  DSP-TORCD        TO  TOK-F01.
     PERFORM  RD-HTOKMS-SEC.
     IF  FG-HTOKMS-INV = 1
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TORCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TORCD
         GO TO  TORCD-CHK-EXIT
     END-IF.
*
     MOVE  TOK-F02          TO  DSP-TORNM.
*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  TORCD-CHK-EXIT
     END-IF.
*
     MOVE  3             TO  PSW.
*
 TORCD-CHK-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.
*
     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.
*
 RD-HTOKMS-EXIT.
     EXIT.
****************************************************************
*  実納品日入力処理  ( PSW = 3 )                               *
****************************************************************
 DSP-JNOHBI-SEC           SECTION.
     MOVE  "DSP-BASHO-SEC"  TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  JNOHBI-CHK-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*      取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW
*      項目戻し
       WHEN  "F006"
         MOVE  "2"          TO  PSW
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.
*
 DSP-JNOHBI-EXIT.
     EXIT.
****************************************************************
*  実納品日チェック                                            *
****************************************************************
 JNOHBI-CHK-SEC        SECTION.
     MOVE  "JNOHBI-CHK-SEC" TO  S-NAME.
*
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JNOHBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JNOHBI.
*
* 実納品日
     IF  DSP-JNOHBI = SPACE OR ZERO
         GO TO  JNOHBI-CHK2-EXIT
     END-IF.
*
     MOVE  "2"             TO  LINK-IN-KBN.
     MOVE  DSP-JNOHBI      TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-JNOHBI
         MOVE  "R"          TO  EDIT-OPTION OF DSP-JNOHBI
         GO TO  JNOHBI-CHK2-EXIT
     END-IF.
*
 JNOHBI-CHK2-EXIT.
*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  JNOHBI-CHK-EXIT
     END-IF.
*
     MOVE  4             TO  PSW.
*
 JNOHBI-CHK-EXIT.
     EXIT.
****************************************************************
*  伝票Ｎｏ入力処理  ( PSW = 4 )                               *
****************************************************************
 DSP-DENNO-SEC         SECTION.
     MOVE  "DSP-DENNO-SEC"  TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  DENNO-CHK-SEC
*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*      取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW
*      項目戻し
       WHEN  "F006"
         MOVE  "3"          TO  PSW
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.
*
 DSP-DENNO-EXIT.
     EXIT.
****************************************************************
*  伝票Ｎｏチェック                                            *
****************************************************************
 DENNO-CHK-SEC         SECTION.
     MOVE  "DENNO-CHK-SEC"  TO  S-NAME.
*
     MOVE  ZERO             TO  WK-IN-GYOSU.
     MOVE  ZERO             TO  WK-INMAX-GYOSU.
     MOVE  1                TO  FG-DENALL-SPACE.
*ナフコの場合、管理番号の入力チェックを行なう。
*#2020/03/08 NAV ST ナフコ関係は停止する
     MOVE       ZERO        TO  DSP-KANRNO
*2013/01/25 NAV ST
*****IF  DSP-TORCD  NOT =  137607
*        MOVE   ZERO        TO  DSP-KANRNO
*    END-IF.
*    IF  DSP-KANRNO  NOT NUMERIC
*        MOVE   ZERO        TO  DSP-KANRNO
*    END-IF.
*    IF  DSP-TORCD  =  137607
*        IF  DSP-KANRNO  >  89999999
*            CONTINUE
*        ELSE
*            IF  ERR-FLG = ZERO
*                MOVE  16        TO  ERR-FLG
*            END-IF
*            MOVE  "C"  TO  EDIT-CURSOR OF DSP-KANRNO
*            MOVE  "R"  TO  EDIT-OPTION OF DSP-KANRNO
*            GO TO  DENNO-CHK2-EXIT
*        END-IF
*****END-IF.
*#2020/03/08 NAV ED ナフコ関係は停止する
*2013/01/25 NAV SED
*
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 5
       *> 画面伝票Ｎｏ項目チェック
       PERFORM  DENNO-CHKB-SEC

     END-PERFORM.
*
     IF  WK-IN-GYOSU NOT = WK-INMAX-GYOSU *> 歯抜けチェク
         IF      (DSP-SDENNO (1) = SPACE OR ZERO)
             AND (DSP-EDENNO (1) = SPACE OR ZERO)
             *> 先頭行が空白で入力がある。
             IF  ERR-FLG = ZERO
                 MOVE  10   TO  ERR-FLG
             END-IF
         ELSE
             IF  ERR-FLG = ZERO
                 MOVE  14   TO  ERR-FLG
             END-IF
         END-IF
*
         PERFORM  VARYING IX  FROM 1 BY 1
                  UNTIL   IX > WK-INMAX-GYOSU

           IF      (DSP-SDENNO (IX) = SPACE OR ZERO)
               AND (DSP-EDENNO (IX) = SPACE OR ZERO)
               MOVE  "C"    TO  EDIT-CURSOR OF DSP-SDENNO (IX)
               MOVE  "R"    TO  EDIT-OPTION OF DSP-SDENNO (IX)
               MOVE  "R"    TO  EDIT-OPTION OF DSP-EDENNO (IX)
           END-IF

         END-PERFORM
*
         GO TO  DENNO-CHK2-EXIT
     END-IF.
*
     IF  ERR-FLG NOT = ZERO
         GO TO  DENNO-CHK2-EXIT
     END-IF.
*
* 2011/12/06,S  S.I/NAV 無指定は全件対象とする。
**     IF  WK-IN-GYOSU = ZERO *> １件も入力が無い
**         IF  ERR-FLG = ZERO
**             MOVE  13        TO  ERR-FLG
**         END-IF
**         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SDENNO (1)
**         MOVE  "R"          TO  EDIT-OPTION OF DSP-SDENNO (1)
**         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDENNO (1)
**         GO TO  DENNO-CHK2-EXIT
**     END-IF.
* 2011/12/06,E  S.I/NAV
*
     IF  ERR-FLG NOT = ZERO
         GO TO  DENNO-CHK2-EXIT
     END-IF.
*
     IF  FG-DENALL-SPACE = 1
         *> 全行未指定の場合全件対象
         *> チェックのため一時的に画面に設定する。
         MOVE  ZERO         TO  DSP-SDENNO (1)
         MOVE  ALL "9"      TO  DSP-EDENNO (1)
         MOVE  1            TO  WK-IN-GYOSU *> 入力行数
         MOVE  1            TO  IX
     END-IF.
*
     PERFORM  DENNO-CHKB2-SEC. *> データ存在チェック
*
 DENNO-CHK2-EXIT.
     IF  FG-DENALL-SPACE = 1
         *> 全行未指定の場合全件対象、元に戻す。
         MOVE  SPACE        TO  DSP-SDENNO (1)(1:)
         MOVE  SPACE        TO  DSP-EDENNO (1)(1:)
     END-IF.
*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  DENNO-CHK-EXIT
     END-IF.
*
     MOVE  12               TO  ERR-FLG.
     MOVE  5                TO  PSW.
*
 DENNO-CHK-EXIT.
     EXIT.
****************************************************************
*  伝票ＮｏチェックＢ                                          *
****************************************************************
 DENNO-CHKB-SEC         SECTION.
     MOVE  "DENNO-CHKB-SEC" TO  S-NAME.
*
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SDENNO (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SDENNO (IX).
     MOVE  "M"              TO  EDIT-OPTION OF DSP-EDENNO (IX).
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-EDENNO (IX).
*
     IF     (DSP-SDENNO (IX) = SPACE OR ZERO)
        AND (DSP-EDENNO (IX) = SPACE OR ZERO) *> 未入力行

        IF  DSP-SDENNO (IX) = ZERO
            MOVE  SPACE     TO  DSP-SDENNO (IX)(1:9)
        END-IF

        IF DSP-EDENNO (IX) = ZERO
           MOVE  SPACE      TO  DSP-EDENNO (IX)(1:9)
        END-IF

        GO TO  DENNO-CHKB-EXIT
     END-IF.
*
     MOVE  ZERO             TO  FG-DENALL-SPACE. *> 入力行あり
*
     IF  IX > WK-INMAX-GYOSU *> 最大入力行の取得
         MOVE  IX           TO  WK-INMAX-GYOSU
     END-IF.
*
     ADD  1   TO  WK-IN-GYOSU. *> 入力行数の取得
*
     IF     DSP-SDENNO (IX)     = SPACE
        AND DSP-EDENNO (IX) NOT = SPACE *> 開始伝票Ｎｏ未指定
        MOVE  ZERO          TO  DSP-SDENNO (IX)(1:9)
     END-IF.
*
     IF     DSP-SDENNO (IX) NOT = SPACE
        AND DSP-EDENNO (IX)     = SPACE *> 終了伝票Ｎｏ未指定
        MOVE  ALL "9"       TO  DSP-EDENNO (IX)(1:9)
     END-IF.
*
     IF  DSP-SDENNO (IX) > DSP-EDENNO(IX)
         IF  ERR-FLG = ZERO
             MOVE  7        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SDENNO (IX)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SDENNO (IX)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-EDENNO (IX)
         GO TO  DENNO-CHKB-EXIT
     END-IF.
*
 DENNO-CHKB-EXIT.
     EXIT.
****************************************************************
*  伝票ＮｏチェックＢ２                                        *
****************************************************************
 DENNO-CHKB2-SEC       SECTION.
     MOVE  "DENNO-CHKB2-SEC"  TO  S-NAME.
*
* 初期値設定
     MOVE  ZERO             TO  FG-NOHNBI-SENTO.
     MOVE  ZERO             TO  FG-NOHNBI-CHK.
     MOVE  ZERO             TO  WK-NOHNBI.
*
     INITIALIZE  FG-DENNO-ARI-MS-G.
     INITIALIZE  URI-REC.
     MOVE  LOW-VALUE        TO  FG-SHTDENF-END.
*
     PERFORM  RD-SHTDENF-SEC.  *> 売上伝票の初期読込み
     IF  FG-SHTDENF-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  8     TO  ERR-FLG
         END-IF
         PERFORM  VARYING IX  FROM 1 BY 1
                  UNTIL   IX > WK-IN-GYOSU
           MOVE  "C"   TO  EDIT-CURSOR OF DSP-SDENNO (IX)
           MOVE  "R"   TO  EDIT-OPTION OF DSP-SDENNO (IX)
           MOVE  "R"   TO  EDIT-OPTION OF DSP-EDENNO (IX)
         END-PERFORM
         GO TO  DENNO-CHKB2-EXIT
     END-IF.
*
     PERFORM  DENNO-CHKC2-SEC.
*
 DENNO-CHKB2-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込み　                                  *
****************************************************************
 RD-SHTDENF-SEC          SECTION.
     MOVE  "RD-SHTDENF-SEC" TO  S-NAME.
*
     IF  FG-SHTDENF-END = LOW-VALUE
         MOVE   SPACE            TO       URI-REC
         INITIALIZE                       URI-REC
         MOVE   DSP-TORCD        TO       URI-F01
         MOVE   DSP-BASHO        TO       URI-F08
         MOVE   DSP-SDENNO(IX)   TO       URI-F02
         START  SHTDENF  KEY >=  URI-F277 *> 売上データ作成
                                 URI-F274 *> オンライン
                                 URI-F01  *> 取引先コード
                                 URI-F08  *> 出荷場所
                                 URI-F02  *> 伝票番号
                                 URI-F04  *> 相殺区分
                                 URI-F051 *> 伝区コード
                                 URI-F07  *> 店舗コード
                                 URI-F112 *> 納品日
                                 URI-F03  *> 行番号
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
 READ-010.
* 売上データ作成区分
     IF  URI-F277 NOT = ZERO *>計上済みデータ
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
 READ-020.
* オンライン区分
     IF  URI-F274 NOT = ZERO *>オンライン伝票
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
 READ-030.
* 取引先コードチェック
     IF  URI-F01  NOT = DSP-TORCD *>取引先コード
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
 READ-040.
* 出荷場所チェック
     IF  URI-F08  NOT = DSP-BASHO *>出荷場所
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
 READ-050.
*
* URI-F01 : 取引先コード
* URI-F02 : 伝票番号
* URI-F48 : 出荷場所
*
* 取引先、出荷場所
******F      URI-F01 = DSP-TORCD
*********2012/07/26↓振分倉庫ではなく、出荷場所へ変更
*********          　手書の場合は、出荷場所で判定する。
*********AND URI-F08 = DSP-BASHO
*T       AND URI-F48 = DSP-BASHO
*********2012/07/26↑振分倉庫ではなく、出荷場所へ変更
     IF  URI-F04  =  ZERO
     AND URI-F051 =  41
     AND URI-F15  >  ZERO
         CONTINUE
     ELSE
         GO TO  RD-SHTDENF-SEC
     END-IF.
 READ-060.
*2013/01/25 NAV ST 管理番号のチェック
     IF  DSP-TORCD  =  137607
         IF  URI-F413  =  DSP-KANRNO
             CONTINUE
         ELSE
             GO TO  RD-SHTDENF-SEC
         END-IF
     END-IF.
*2013/01/25 NAV ED 管理番号のチェック
*
* 伝票番号
     PERFORM  RD-SHTDENFB-SEC.
     IF  FG-DENNO-ARI = ZERO
         GO TO  RD-SHTDENF-SEC
     END-IF.
*
 RD-SHTDENF-EXIT.
     EXIT.
****************************************************************
*  売上伝票ファイル読込みＢ                                    *
****************************************************************
 RD-SHTDENFB-SEC          SECTION.
     MOVE  "RD-SHTDENFB-SEC"  TO  S-NAME.
*
     MOVE  ZERO             TO  FG-DENNO-ARI.
*
     PERFORM  VARYING  IX-DENNO  FROM 1 BY 1
              UNTIL    IX-DENNO > WK-IN-GYOSU
       PERFORM  RD-SHTDENFC-SEC
     END-PERFORM.
*
 RD-SHTDENFB-EXIT.
     EXIT.
****************************************************************
*  売上伝票ファイル読込みＣ                                    *
****************************************************************
 RD-SHTDENFC-SEC          SECTION.
     MOVE  "RD-SHTDENFC-SEC"  TO  S-NAME.
*
     IF      DSP-SDENNO (IX-DENNO) = SPACE
         AND DSP-EDENNO (IX-DENNO) = SPACE *> 伝票Ｎｏ未指定行

         IF  FG-DENNO-ARI-MS (IX-DENNO) = ZERO
             MOVE  9  TO  FG-DENNO-ARI-MS (IX-DENNO)
         END-IF

         IF  FG-DENNO-ARI-KOURI-RENKEI (IX-DENNO) = ZERO
             MOVE  9  TO  FG-DENNO-ARI-KOURI-RENKEI (IX-DENNO)
         END-IF

         GO TO  RD-SHTDENFC-EXIT
     END-IF.
*
     IF      URI-F02 >= DSP-SDENNO (IX-DENNO)
         AND URI-F02 <= DSP-EDENNO (IX-DENNO)  *>条件範囲あり
         CONTINUE
     ELSE
         GO TO  RD-SHTDENFC-EXIT
     END-IF.
*
     MOVE  1                TO  FG-DENNO-ARI.
*
     IF  FG-DENNO-ARI-MS (IX-DENNO) = ZERO
         MOVE  1      TO  FG-DENNO-ARI-MS (IX-DENNO)
     END-IF.
*↓2012/07/13 先行受注区分は無視する
*  ↓2016/10/20 ST 先行受注区分＝１の明細のみ連携に変更
     IF  URI-F32 = RENKBN  AND  *> 先行受注区分あり
         URI-F33 = "         "  *> 先行受注区分あり
         CONTINUE
*****IF  URI-F33 = "         "  *> 先行受注区分あり
*****    CONTINUE
*  ↑2016/10/20 ED 先行受注区分＝１の明細のみ連携に変更
*↑2012/07/13
     ELSE
         GO TO  RD-SHTDENFC-EXIT
     END-IF.
*
     IF  FG-DENNO-ARI-KOURI-RENKEI (IX-DENNO) = ZERO
         MOVE  1  TO  FG-DENNO-ARI-KOURI-RENKEI (IX-DENNO)
     END-IF.
*
     IF  FG-NOHNBI-SENTO = ZERO
         MOVE  1            TO  FG-NOHNBI-SENTO
         MOVE  URI-F112     TO  WK-NOHNBI
     ELSE
         IF  URI-F112 NOT = WK-NOHNBI  *> 複数納品日あり
             MOVE  1        TO  FG-NOHNBI-CHK
         END-IF
     END-IF.
*
 RD-SHTDENFC-EXIT.
     EXIT.
****************************************************************
*  伝票ＮｏチェックＣ２                                        *
****************************************************************
 DENNO-CHKC2-SEC       SECTION.
     MOVE  "DENNO-CHKC2-SEC"  TO  S-NAME.
*
     PERFORM  UNTIL FG-SHTDENF-END = "END" *>売上伝票を全件読み
       PERFORM  RD-SHTDENF-SEC
     END-PERFORM.
*
     PERFORM  DENNO-CHKD2-SEC. *> 伝票範囲のデータチェック
     IF  ERR-FLG NOT = ZERO
         GO TO  DENNO-CHKC2-EXIT
     END-IF.
*
     IF  DSP-JNOHBI = SPACE OR ZERO
         *> 実納品日未指定時は売上伝票の納入日を実納入日
         *> として使用する。
         MOVE  WK-NOHNBI   TO  DSP-JNOHBI
     END-IF.
*
     PERFORM  JNOHNBI-HANI-KEISAN-SEC.
     IF     DSP-JNOHBI < WK-JNOHNBI-ST
         OR DSP-JNOHBI > WK-JNOHNBI-ED
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-JNOHBI
         MOVE  "R"          TO  EDIT-OPTION OF DSP-JNOHBI
         MOVE  3            TO  PSW *> 実納入日入力に戻す。
         GO TO  DENNO-CHKC2-EXIT
     END-IF.
*
 DENNO-CHKC2-EXIT.
     EXIT.
****************************************************************
*  実納品日入力可期間計算                                      *
****************************************************************
 JNOHNBI-HANI-KEISAN-SEC    SECTION.
     MOVE  "JNOHNBI-HANI-KEISAN-SEC"  TO  S-NAME.
*
*    実納品日の入力可能期間は
*    「納品日を基準にし過去１週間～将来１ヵ月間」
*
     MOVE  ZERO             TO  WK-JNOHNBI-ST.
     MOVE  ZERO             TO  WK-JNOHNBI-ED.
*  期間開始日
     MOVE  WK-NOHNBI        TO  WK-YMD.
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
*
     PERFORM  HZK-CVT2-SEC.
     MOVE  WK-YMD           TO  WK-JNOHNBI-ST.
*
*  期間終了日
     MOVE  WK-NOHNBI        TO  WK-YMD.
     PERFORM  HZK-CVT-SEC.
     COMPUTE  WK-CMPYMD-M = WK-CMPYMD-M + 1.
     IF  WK-CMPYMD-M > 12
         COMPUTE  WK-CMPYMD-M = WK-CMPYMD-M - 12
         COMPUTE  WK-CMPYMD-Y = WK-CMPYMD-Y + 1
     END-IF.
     PERFORM  HZK-CVT2-SEC.
     MOVE  WK-YMD           TO  WK-JNOHNBI-ED.
*
 JNOHNBI-HANI-KEISAN-EXIT.
     EXIT.
****************************************************************
*  日付変換                                                    *
****************************************************************
 HZK-CVT-SEC    SECTION.
     MOVE  "HZK-CVT-SEC"    TO  S-NAME.
*
     MOVE  WK-YMD-Y         TO  WK-CMPYMD-Y.
     MOVE  WK-YMD-M         TO  WK-CMPYMD-M.
     MOVE  WK-YMD-D         TO  WK-CMPYMD-D.
*
 HZK-CVT-EXIT.
     EXIT.
****************************************************************
*  月末日計算                                                  *
****************************************************************
 MATUBI-KEISAN-SEC    SECTION.
     MOVE  "MATUBI-KEISAN-SEC"  TO  S-NAME.
*
     MOVE  ZERO             TO  WK-MATUBI.
*
     EVALUATE  TRUE
       WHEN  WK-CMPYMD-M = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
         MOVE 31            TO  WK-CMPYMD-D
       WHEN  WK-CMPYMD-M = 4 OR 6 OR 9 OR 11
         MOVE 30            TO  WK-CMPYMD-D
       WHEN  OTHER
         PERFORM  URUUTUKI-KEISAN-SEC
     END-EVALUATE.
*
 MATUBI-KEISAN-EXIT.
     EXIT.
****************************************************************
*  閏月末日計算                                                *
****************************************************************
 URUUTUKI-KEISAN-SEC    SECTION.
     MOVE  "URUUTUKI-KEISAN-SEC"  TO  S-NAME.
*
     MOVE  28               TO  WK-CMPYMD-D.
*
     DIVIDE  4   INTO  WK-CMPYMD-Y
         GIVING    WK-SYO
         REMAINDER WK-AMARI.
*
     IF  WK-AMARI NOT = ZERO *> 平年
         GO TO  URUUTUKI-KEISAN-EXIT
     END-IF.
*
     DIVIDE  100   INTO  WK-CMPYMD-Y
         GIVING    WK-SYO
         REMAINDER WK-AMARI.
*
     IF  WK-AMARI NOT = ZERO *> 閏年
         COMPUTE  WK-CMPYMD-D = WK-CMPYMD-D + 1
         GO TO  URUUTUKI-KEISAN-EXIT
     END-IF.
*
     DIVIDE  400   INTO  WK-CMPYMD-Y
         GIVING    WK-SYO
         REMAINDER WK-AMARI.
*
     IF  WK-AMARI NOT = ZERO *> 平年
         CONTINUE
     ELSE                *> 閏年
         COMPUTE  WK-CMPYMD-D = WK-CMPYMD-D + 1
     END-IF.
*
 URUUTUKI-KEISAN-EXIT.
     EXIT.
****************************************************************
*  日付変換                                                    *
****************************************************************
 HZK-CVT2-SEC    SECTION.
     MOVE  "HZK-CVT2-SEC"   TO  S-NAME.
*
     MOVE  WK-CMPYMD-Y      TO  WK-YMD-Y.
     MOVE  WK-CMPYMD-M      TO  WK-YMD-M.
     MOVE  WK-CMPYMD-D      TO  WK-YMD-D.
*
 HZK-CVT2-EXIT.
     EXIT.
****************************************************************
*  伝票ＮｏチェックＤ２                                        *
****************************************************************
 DENNO-CHKD2-SEC       SECTION.
     MOVE  "DENNO-CHKD2-SEC"  TO  S-NAME.
*
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > WK-IN-GYOSU
                           *> 伝票範囲のデータチェック
       PERFORM  DENNO-CHKE2-SEC
     END-PERFORM.
*
 DENNO-CHKD2-EXIT.
     EXIT.
****************************************************************
*  伝票ＮｏチェックＥ２                                        *
****************************************************************
 DENNO-CHKE2-SEC       SECTION.
     MOVE  "DENNO-CHKE2-SEC"  TO  S-NAME.
*
     IF  FG-DENNO-ARI-MS (IX) = ZERO *> データ範囲なし
         IF  ERR-FLG = ZERO
             MOVE  8        TO  ERR-FLG
         END-IF
         MOVE  "C"    TO  EDIT-CURSOR OF DSP-SDENNO (IX)
         MOVE  "R"    TO  EDIT-OPTION OF DSP-SDENNO (IX)
         MOVE  "R"    TO  EDIT-OPTION OF DSP-EDENNO (IX)
         GO TO  DENNO-CHKE2-EXIT
     END-IF.
*
     IF  FG-DENNO-ARI-KOURI-RENKEI (IX) = ZERO *> データなし
         IF  ERR-FLG = ZERO
             MOVE  11       TO  ERR-FLG
         END-IF
         MOVE  "C"    TO  EDIT-CURSOR OF DSP-SDENNO (IX)
         MOVE  "R"    TO  EDIT-OPTION OF DSP-SDENNO (IX)
         MOVE  "R"    TO  EDIT-OPTION OF DSP-EDENNO (IX)
         GO TO  DENNO-CHKE2-EXIT
     END-IF.
*
     IF  FG-NOHNBI-CHK = 1 *>納品日複数
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF

         IF      (DSP-SDENNO (IX) = SPACE OR ZERO)
             AND (DSP-EDENNO (IX) = SPACE OR ZERO)
             CONTINUE
         ELSE
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SDENNO (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SDENNO (IX)
             MOVE  "R"      TO  EDIT-OPTION OF DSP-EDENNO (IX)
         END-IF
     END-IF.
*
 DENNO-CHKE2-EXIT.
     EXIT.
****************************************************************
*  確認処理入力 （ PSW = 5 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
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
         MOVE  "4"          TO  PSW
*    取消
       WHEN  "F004"
         PERFORM  INIT-DSP-SEC
         MOVE  "1"          TO  PSW
       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.
     MOVE  "PARA-OT-SEC"    TO  S-NAME.
*
* 部門・担当者ＣＤ
     MOVE  SPACE            TO  PAA-TANCD.
     STRING
       PAR-BMNCD-IN  PAR-TANCD-IN
       DELIMITED BY SIZE  INTO  PAA-TANCD.
* オンライン/手書種別
     MOVE  "1"              TO  PAA-ONLTEG.
* バッチＮｏ
     MOVE  ZERO             TO  PAA-BATNO.
* 出荷場所
     MOVE  DSP-BASHO        TO  PAA-BASHO.
* 納品日
     MOVE  WK-NOHNBI        TO  PAA-NOHNBI.
* 実納品日
     MOVE  DSP-JNOHBI       TO  PAA-JNOHNBI.
* 取引先ＣＤ
     MOVE  DSP-TORCD        TO  PAA-TORCD.

     IF  FG-DENALL-SPACE = 1 *> 全行未指定の場合全件対象
         *> 伝票Ｎｏ１（ＦＲＯＭ）
         MOVE  ZERO              TO  PAA-DEN1-FROM
         *> 伝票Ｎｏ１（ＴＯ）
         MOVE  ALL "9"           TO  PAA-DEN1-TO
     ELSE                    *> 通常の指定の場合
         *> 伝票Ｎｏ１（ＦＲＯＭ）
         MOVE  DSP-SDENNO (1)    TO  PAA-DEN1-FROM
         *> 伝票Ｎｏ１（ＴＯ）
         MOVE  DSP-EDENNO (1)    TO  PAA-DEN1-TO
     END-IF.
* 伝票Ｎｏ２（ＦＲＯＭ）
     IF  DSP-SDENNO (2) IS NUMERIC
         MOVE  DSP-SDENNO (2)    TO  PAA-DEN2-FROM
     ELSE
         MOVE  ZERO              TO  PAA-DEN2-FROM
     END-IF.
* 伝票Ｎｏ２（ＴＯ）
     IF  DSP-EDENNO (2) IS NUMERIC
         MOVE  DSP-EDENNO (2)    TO  PAA-DEN2-TO
     ELSE
         MOVE  ZERO              TO  PAA-DEN2-TO
     END-IF.
* 伝票Ｎｏ３（ＦＲＯＭ）
     IF  DSP-SDENNO (3) IS NUMERIC
         MOVE  DSP-SDENNO (3)    TO  PAA-DEN3-FROM
     ELSE
         MOVE  ZERO              TO  PAA-DEN3-FROM
     END-IF.
* 伝票Ｎｏ３（ＴＯ）
     IF  DSP-EDENNO (3) IS NUMERIC
         MOVE  DSP-EDENNO (3)    TO  PAA-DEN3-TO
     ELSE
         MOVE  ZERO              TO  PAA-DEN3-TO
     END-IF.
* 伝票Ｎｏ４（ＦＲＯＭ）
     IF  DSP-SDENNO (4) IS NUMERIC
         MOVE  DSP-SDENNO (4)    TO  PAA-DEN4-FROM
     ELSE
         MOVE  ZERO              TO  PAA-DEN4-FROM
     END-IF.
* 伝票Ｎｏ４（ＴＯ）
     IF  DSP-EDENNO (4) IS NUMERIC
         MOVE  DSP-EDENNO (4)    TO  PAA-DEN4-TO
     ELSE
         MOVE  ZERO              TO  PAA-DEN4-TO
     END-IF.
* 伝票Ｎｏ５（ＦＲＯＭ）
     IF  DSP-SDENNO (5) IS NUMERIC
         MOVE  DSP-SDENNO (5)    TO  PAA-DEN5-FROM
     ELSE
         MOVE  ZERO              TO  PAA-DEN5-FROM
     END-IF.
* 伝票Ｎｏ５（ＴＯ）
     IF  DSP-EDENNO (5) IS NUMERIC
         MOVE  DSP-EDENNO (5)    TO  PAA-DEN5-TO
     ELSE
         MOVE  ZERO              TO  PAA-DEN5-TO
     END-IF.
* 状態
     MOVE  SPACE            TO  PAA-STS.

* 連携Ｎｏ
     MOVE  ZERO             TO  PAA-RENKEINO.
*2013/01/29 NAV ST
*#2020/03/08 NAV ST ナフコ関係は停止する
*****IF    DSP-TORCD  =  137607
*****      MOVE DSP-KANRNO  TO  PAA-KANRINO
*****ELSE
           MOVE ZERO        TO  PAA-KANRINO.
*****END-IF.
*#2020/03/08 NAV ED ナフコ関係は停止する
*2013/01/29 NAV ED
* オーダー区分
     MOVE  DSP-ORDKBN       TO  PAA-ORDER-KBN.
*
 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  SHTDENF.
     CLOSE  ZSOKMS.
     CLOSE  HTOKMS.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SNA0031I   END PROGRAM  >>******************

```
