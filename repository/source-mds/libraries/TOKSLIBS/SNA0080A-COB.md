# SNA0080A

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0080A.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：小売連携発注データ照会（詳細）
*    作成日／更新日　：2011/11/10
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      小売連携したｵﾝﾗｲﾝ発注ﾃﾞｰﾀの状態を確認（苗側取込済、
*      未済や取消済・未済など）するため、選択画面にて指定
*      された連携Ｎｏより、詳細情報を照会する。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0080A.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/11/10.
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

*小売連携累積ファイル
     SELECT  NARRUIF
       ASSIGN    TO    NARRUIL2
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
         RUI-F01 *> 連携Ｎｏ
         RUI-F08 *> 納品日
         RUI-F09 *> 実納品日
         RUI-F13 *> 相手伝票番号
         RUI-F17 *> 伝票行番号
       FILE STATUS     RUI-ST.

*連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         KAN-F01  *> 連携Ｎｏ
       FILE STATUS     KAN-ST.

*取引先マスタ
     SELECT  HTOKMS
       ASSIGN    TO    TOKMS2
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TOK-F01 *> 相手取引先コード
       FILE STATUS     TOK-ST.

*担当者マスタ
     SELECT  HTANMS
       ASSIGN    TO    TANMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TAN-F01 *> 部門ＣＤ
         TAN-F02 *> 担当者ＣＤ
       FILE STATUS     TAN-ST.

*商品名称マスタ
     SELECT  HMEIMS
       ASSIGN    TO    MEIMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         MEI-F011  *> 商品コード
         MEI-F0121 *> 単１
         MEI-F0122 *> 単２
         MEI-F0123 *> 単３
       FILE STATUS     MEI-ST.

*倉庫マスタ
     SELECT  ZSOKMS
       ASSIGN    TO        ZSOKMS1
       ORGANIZATION        INDEXED
       ACCESS    MODE      RANDOM
       RECORD    KEY
         SOK-F01 *> 倉庫コード
       FILE      STATUS    SOK-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
     LABEL     RECORD    IS   OMITTED.
     COPY      FNA00801  OF   XMDLIB
     JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 小売連携累積ファイル                               *
****************************************************************
 FD  NARRUIF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARRUIF   OF   XFDLIB
     JOINING   RUI       AS   PREFIX.
****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKANF   OF   XFDLIB
     JOINING   KAN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTOKMS    OF   XFDLIB
     JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 担当者マスタ                                       *
****************************************************************
 FD  HTANMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HTANMS    OF   XFDLIB
     JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS
     LABEL     RECORD    IS   STANDARD.
     COPY      HMEIMS   OF   XFDLIB
     JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                         *
****************************************************************
 FD  ZSOKMS
     LABEL     RECORD    IS   STANDARD.
     COPY      ZSOKMS    OF   XFDLIB
     JOINING   SOK       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST             PIC  X(02).
     03  RUI-ST             PIC  X(02).
     03  KAN-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  TAN-ST             PIC  X(02).
     03  MEI-ST             PIC  X(02).
     03  SOK-ST             PIC  X(02).
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
     03  FG-NARRUIF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-NARKANF-INV     PIC  9(01)  VALUE  ZERO.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HTANMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HMEIMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-ZSOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-TBL-MAXOVER     PIC  9(01).
*ワーク領域
 01  WRK-AREA.
     03  S-NAME-SV          PIC  X(30).
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
     03  CT-SEQ             PIC  9(03).
     03  CT-PAGE            PIC  9(04).
     03  IX                 PIC  9(04).
     03  IX-TBL             PIC  9(04).
     03  IX-GYO             PIC  9(03).
     03  CT-SEL             PIC  9(03).
     03  FG-SEL             PIC  9(01).
     03  WK-SELGYO          PIC  9(02).
     03  WK-SDENNO          PIC  9(09).

 01  WK-MIDASHI.
     03  WK-MIDASHI-ONL     PIC  N(22)  VALUE
     NC"バッチＮｏ　　　　　　　　　　場所　納品日　".
     03  WK-MIDASHI-TEG     PIC  N(22)  VALUE
     NC"場所　抽出取引先　抽出伝票Ｎｏ範囲　　　　　".

 01  WK-KEY-ONL.
     03  WK-KEY-ONL-BATHZK  PIC  9(08).
     03  FILLER             PIC  X(03)  VALUE " - ".
     03  WK-KEY-ONL-BATHM   PIC  9(04).
     03  FILLER             PIC  X(03)  VALUE " - ".
     03  WK-KEY-ONL-BATTOR  PIC  9(08).
     03  FILLER             PIC  X(05)  VALUE SPACE.
     03  WK-KEY-ONL-BASHO   PIC  X(02).
     03  FILLER             PIC  X(02)  VALUE SPACE.
     03  WK-KEY-ONL-NOHNBI  PIC  9(08).

 01  WK-KEY-TEG.
     03  FILLER             PIC  X(01)  VALUE SPACE.
     03  WK-KEY-TEG-BASHO   PIC  X(02).
     03  FILLER             PIC  X(04)  VALUE SPACE.
     03  WK-KEY-TEG-SELTOR  PIC  9(08).
     03  FILLER             PIC  X(04)  VALUE SPACE.
     03  WK-KEY-TEG-SELSDEN PIC  9(09).
     03  FILLER             PIC  X(05)  VALUE "  -  ".
     03  WK-KEY-TEG-SELEDEN PIC  9(09).

* 画面表示データ格納領域
 01  TB-DT.
     03  TB-DT-G.
       05  TB-DT-G2  OCCURS 99. *> ページ
         07  TB-DT-G2  OCCURS 6. *> 行
           09  TB-NOHNBI    PIC  9(08).
           09  TB-JNOHBI    PIC  9(08).
           09  TB-DENNO     PIC  9(09).
           09  TB-GYO       PIC  9(02).
           09  TB-SHOCD     PIC  X(08).
           09  TB-HINTAN    PIC  X(08).
           09  TB-TENCD     PIC  9(05).
           09  TB-SURYO     PIC S9(07).
           09  TB-BASHO     PIC  X(02).

     03  TB-DT-GR  REDEFINES TB-DT-G.
       05  TB-DT-G2R  OCCURS 594.
         07  TB-NOHNBI-R    PIC  9(08).
         07  TB-JNOHBI-R    PIC  9(08).
         07  TB-DENNO-R     PIC  9(09).
         07  TB-GYO-R       PIC  9(02).
         07  TB-SHOCD-R     PIC  X(08).
         07  TB-HINTAN-R    PIC  X(08).
         07  TB-TENCD-R     PIC  9(05).
         07  TB-SURYO-R     PIC S9(07).
         07  TB-BASHO-R     PIC  X(02).

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
       05  FILLER           PIC  N(30)
       VALUE NC"_終了　_前頁　_次頁".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(30)  OCCURS 1.

*メッセージの取得
*A---B-------2---------3---------4---------5---------6---------7E*
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER         PIC  N(30)  VALUE
         NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER         PIC  N(30)  VALUE
         NC"前ページはありません。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
         NC"次ページはありません。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"データはありますが、これ以上のスクロールはできません
-          "。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
         NC"指定された条件での表示対象データはありません。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG11.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG12.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
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
     03  RUI-ERR           PIC  N(20)  VALUE
         NC"小売連携累積ファイルエラー".
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  TAN-ERR           PIC  N(20)  VALUE
         NC"担当者マスタエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SOK-ERR           PIC  N(20)  VALUE
         NC"倉庫マスタエラー".

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

* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION
                           USING  PAA-TANCD
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
 RUI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARRUIF.
     DISPLAY     RUI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RUI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANF.
     DISPLAY     KAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
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

*I** STOP RUN.
     EXIT     PROGRAM.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
     PERFORM  SDATE-GET-SEC.
* ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  INPUT NARRUIF.
     OPEN  INPUT NARKANF.
     OPEN  INPUT HTOKMS.
     OPEN  INPUT HTANMS.
     OPEN  INPUT HMEIMS.
     OPEN  INPUT ZSOKMS.
* ワークの初期化
     INITIALIZE  FLG-AREA.
     MOVE  ZERO             TO  PSW.
* 初期画面の表示
     MOVE  SPACE            TO  DSP-PRO.
     PERFORM  INIT-DSP-SEC.
* 画面表示データＴＢＬ格納
     PERFORM  TBL-SET-SEC.
* 初期画面編集
     MOVE  1                TO  CT-PAGE.
     PERFORM  GMN-SET-SEC.
* 初期画面へ
     MOVE  "1"              TO  PSW.
*
     MOVE  S-NAME-SV        TO  S-NAME.
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-SEC"       TO  S-NAME.

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

     MOVE  S-NAME-SV        TO  S-NAME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.

*  画面初期化
     MOVE  SPACE            TO  DSP-FNA00801.
*  ＰＧＩＤ
     MOVE  "SNA0080A"       TO  DSP-PGID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.

     MOVE  S-NAME-SV        TO  S-NAME.
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*    表示データ格納ＴＢＬ設定                                  *
****************************************************************
 TBL-SET-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SET-SEC"    TO  S-NAME.

     MOVE  SPACE            TO  TB-DT.
     INITIALIZE  TB-DT.
     MOVE  ZERO             TO  IX-TBL.
     MOVE  ZERO             TO  CT-SEQ.
     MOVE  ZERO             TO  FG-TBL-MAXOVER.

     INITIALIZE  RUI-REC.
     MOVE  PAA-RENKEINO     TO  RUI-F01. *> 連携Ｎｏ
     MOVE  PAA-NOHNBI       TO  RUI-F08. *> 納品日
     MOVE  PAA-JNOHNBI      TO  RUI-F09. *> 実納品日

     MOVE  LOW-VALUE        TO  FG-NARRUIF-END.
     PERFORM  RD-NARRUIF-SEC.

     PERFORM  UNTIL FG-NARRUIF-END = "END"
       PERFORM  TBL-SETB-SEC
       PERFORM  RD-NARRUIF-SEC

     END-PERFORM.

     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SET-EXIT.
     EXIT.
****************************************************************
*    小売連携累積ファイル読込み                                *
****************************************************************
 RD-NARRUIF-SEC    SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-NARRUIF-SEC" TO  S-NAME.

     IF  FG-NARRUIF-END = LOW-VALUE
         START  NARRUIF  KEY >=  RUI-F01 *> 連携Ｎｏ
                                 RUI-F08 *> 納品日
                                 RUI-F09 *> 実納品日
                                 RUI-F13 *> 相手伝票番号
                                 RUI-F17 *> 伝票行番号
           INVALID KEY
              MOVE  "END"   TO  FG-NARRUIF-END
              GO TO  RD-NARRUIF-090
         END-START

         MOVE  SPACE        TO  FG-NARRUIF-END

     END-IF.

     READ  NARRUIF
       AT  END
         MOVE  "END"        TO  FG-NARRUIF-END
         GO TO  RD-NARRUIF-090
     END-READ.

* 連携Ｎｏ
     IF  RUI-F01 = PAA-RENKEINO
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-NARRUIF-END
         GO TO  RD-NARRUIF-090
     END-IF.

 RD-NARRUIF-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 RD-NARRUIF-EXIT.
     EXIT.
****************************************************************
*    表示データ格納ＴＢＬ設定Ｂ                                *
****************************************************************
 TBL-SETB-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "TBL-SETB-SEC"   TO  S-NAME.

     IF  IX-TBL >= 594
         MOVE  1            TO  FG-TBL-MAXOVER
         GO TO  TBL-SETB-090
     END-IF.

     ADD  1   TO  IX-TBL.
     MOVE  IX-TBL           TO  CT-SEQ.

     MOVE  RUI-F08          TO  TB-NOHNBI-R (IX-TBL).
     MOVE  RUI-F09          TO  TB-JNOHBI-R (IX-TBL).
*I***MOVE  RUI-F12          TO  TB-DENNO-R  (IX-TBL).
     MOVE  RUI-F13          TO  TB-DENNO-R  (IX-TBL).
     MOVE  RUI-F17          TO  TB-GYO-R    (IX-TBL).
     MOVE  RUI-F19          TO  TB-SHOCD-R  (IX-TBL).
     MOVE  RUI-F20          TO  TB-HINTAN-R (IX-TBL).
     MOVE  RUI-F10          TO  TB-TENCD-R  (IX-TBL).
     MOVE  RUI-F22          TO  TB-SURYO-R  (IX-TBL).
     MOVE  RUI-F18          TO  TB-BASHO-R  (IX-TBL).

 TBL-SETB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 TBL-SETB-EXIT.
     EXIT.
****************************************************************
*    画面編集処理                                              *
****************************************************************
 GMN-SET-SEC           SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GMN-SET-SEC"    TO  S-NAME.

* オンライン/手書き
     IF  PAA-ONLTEG = "1" *> 手書き
         MOVE  NC"　手　書　"    TO  DSP-ONLTEG
     ELSE                 *> オンライン
         MOVE  NC"オンライン"    TO  DSP-ONLTEG
     END-IF.
* 連携Ｎｏ
     MOVE  PAA-RENKEINO     TO  DSP-RENNO.

* 連携Ｎｏ管理テーブル
     MOVE  PAA-RENKEINO     TO  KAN-F01.
     PERFORM  RD-NARKANF-SEC.
     IF  FG-NARKANF-INV = ZERO
         MOVE  PAA-RENKEINO TO  DSP-RENNO
       *> 連携Ｎｏの色を設定する。
         EVALUATE  TRUE
           WHEN  KAN-F04 = "1"  *> 未送信、赤
             MOVE  "R"      TO  EDIT-COLOR OF DSP-RENNO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-RENNO
           WHEN  KAN-F04 = "2"  *> 苗側未取込、黄
             MOVE  "Y"      TO  EDIT-COLOR OF DSP-RENNO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-RENNO
           WHEN  KAN-F04 = "4"  *> 未取消、青
*I***********MOVE  "B"      TO  EDIT-COLOR OF DSP-RENNO
             MOVE  "W"      TO  EDIT-COLOR OF DSP-RENNO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-RENNO
           WHEN  KAN-F04 = "5"  *> 取消済み、桃
             MOVE  "P"      TO  EDIT-COLOR OF DSP-RENNO
             MOVE  "R"      TO  EDIT-OPTION OF DSP-RENNO
           WHEN  OTHER          *> 取込済、標準色
             MOVE  "M"      TO  EDIT-COLOR OF DSP-RENNO
             MOVE  "M"      TO  EDIT-OPTION OF DSP-RENNO
         END-EVALUATE
     ELSE
         MOVE  ALL "*"      TO  DSP-RENNO
         MOVE  "M"          TO  EDIT-COLOR OF DSP-RENNO
     END-IF.

   *> キー部編集
     IF  PAA-ONLTEG = "1" *> 手書き
         MOVE  WK-MIDASHI-TEG    TO  DSP-MIDASI
         MOVE  PAA-BASHO         TO  WK-KEY-TEG-BASHO
         MOVE  PAA-TORCD         TO  WK-KEY-TEG-SELTOR
         MOVE  PAA-DEN1-FROM     TO  WK-KEY-TEG-SELSDEN
         MOVE  PAA-DEN1-TO       TO  WK-KEY-TEG-SELEDEN
         MOVE  WK-KEY-TEG        TO  DSP-KEY
     ELSE                 *> オンライン
         MOVE  WK-MIDASHI-ONL    TO  DSP-MIDASI
         MOVE  PAA-BATNO  (01:8) TO  WK-KEY-ONL-BATHZK
         MOVE  PAA-BATNO  (09:4) TO  WK-KEY-ONL-BATHM
         MOVE  PAA-BATNO  (13:8) TO  WK-KEY-ONL-BATTOR
         MOVE  PAA-BASHO         TO  WK-KEY-ONL-BASHO
         MOVE  PAA-NOHNBI        TO  WK-KEY-ONL-NOHNBI
         MOVE  WK-KEY-ONL        TO  DSP-KEY
     END-IF.

     MOVE  PAA-TANCD (5:2)  TO  DSP-TANCD.

* 担当者マスタ
     MOVE  PAA-TANCD (1:4)  TO  TAN-F01. *> 部門ＣＤ
     MOVE  PAA-TANCD (5:2)  TO  TAN-F02. *> 担当者ＣＤ
     PERFORM  RD-HTANMS-SEC.
     IF  FG-HTANMS-INV = ZERO
         MOVE  TAN-F03      TO  DSP-TANNM
     ELSE
         MOVE  ALL NC"＊"   TO  DSP-TANNM
     END-IF.

     MOVE  PAA-TORCD        TO  DSP-TORCD.

* 取引先マスタ
     MOVE  PAA-TORCD        TO  TOK-F01 *> 相手取引先コード
     PERFORM  RD-HTOKMS-SEC.
     IF  FG-HTOKMS-INV = ZERO
         MOVE  TOK-F02      TO  DSP-TORNM
     ELSE
         MOVE  ALL NC"＊"   TO  DSP-TORNM
     END-IF.

* 明細部編集
     IF  CT-SEQ > ZERO
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         GO TO  GMN-SET-090
     END-IF.

     DIVIDE  6   INTO CT-SEQ
       GIVING    WK-SYO
       REMAINDER WK-AMARI.

     MOVE  CT-PAGE      TO  DSP-PG
     IF WK-AMARI = ZERO
        MOVE  WK-SYO    TO  DSP-GPG
     ELSE
        COMPUTE  DSP-GPG = WK-SYO + 1
     END-IF.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 6
       PERFORM  GMN-SETB-SEC
     END-PERFORM.

 GMN-SET-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GMN-SET-EXIT.
     EXIT.
****************************************************************
*    連携Ｎｏ管理テーブル検索                                  *
****************************************************************
 RD-NARKANF-SEC        SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-NARKANF-SEC"  TO  S-NAME.

     READ  NARKANF
       INVALID
         MOVE  1            TO  FG-NARKANF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARKANF-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 RD-NARKANF-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ検索                                          *
****************************************************************
 RD-HTANMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HTANMS-SEC"  TO  S-NAME.

     READ  HTANMS
       INVALID
         MOVE  1            TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTANMS-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HTANMS-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.

     READ  HTOKMS
       INVALID
         MOVE  1            TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTOKMS-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HTOKMS-EXIT.
     EXIT.
****************************************************************
*    画面編集Ｂ処理                                            *
****************************************************************
 GMN-SETB-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "GMN-SETB-SEC"   TO  S-NAME.

     IF      CT-PAGE = DSP-GPG
         AND WK-AMARI NOT = ZERO
         AND IX-GYO > WK-AMARI  *> 最終ページのデータなし行

         MOVE  SPACE        TO  DSP-NOHNBI (IX-GYO)(1:8)
         MOVE  SPACE        TO  DSP-JNOHBI (IX-GYO)
         MOVE  SPACE        TO  DSP-DENNO  (IX-GYO)(1:9)
         MOVE  SPACE        TO  DSP-GYO    (IX-GYO)(1:2)
         MOVE  SPACE        TO  DSP-SHOCD  (IX-GYO)
         MOVE  SPACE        TO  DSP-HNTAN  (IX-GYO)
         MOVE  SPACE        TO  DSP-TENCD  (IX-GYO)(1:5)
         MOVE  SPACE        TO  DSP-SURY   (IX-GYO)(1:7)
         MOVE  SPACE        TO  DSP-BASHO  (IX-GYO)
         MOVE  SPACE        TO  DSP-BASHNM (IX-GYO)
         MOVE  SPACE        TO  DSP-SHONM  (IX-GYO)
         GO TO  GMN-SETB-090
     END-IF.

     MOVE  TB-NOHNBI (CT-PAGE IX-GYO) TO  DSP-NOHNBI (IX-GYO).

     MOVE  SPACE                      TO  DSP-JNOHBI (IX-GYO).
     STRING  "(" TB-JNOHBI (CT-PAGE IX-GYO) (1:4)
             "." TB-JNOHBI (CT-PAGE IX-GYO) (5:2)
             "." TB-JNOHBI (CT-PAGE IX-GYO) (7:2)  ")"
       DELIMITED BY SIZE  INTO  DSP-JNOHBI (IX-GYO).

     MOVE  TB-DENNO  (CT-PAGE IX-GYO) TO  DSP-DENNO  (IX-GYO).
     MOVE  TB-GYO    (CT-PAGE IX-GYO) TO  DSP-GYO    (IX-GYO).
     MOVE  TB-SHOCD  (CT-PAGE IX-GYO) TO  DSP-SHOCD  (IX-GYO).
     MOVE  TB-HINTAN (CT-PAGE IX-GYO) TO  DSP-HNTAN  (IX-GYO).
     MOVE  TB-TENCD  (CT-PAGE IX-GYO) TO  DSP-TENCD  (IX-GYO).
     MOVE  TB-SURYO  (CT-PAGE IX-GYO) TO  DSP-SURY   (IX-GYO).
     MOVE  TB-BASHO  (CT-PAGE IX-GYO) TO  DSP-BASHO  (IX-GYO).

* 倉庫マスタ
     MOVE  TB-BASHO  (CT-PAGE IX-GYO) TO  SOK-F01.
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = ZERO
         MOVE  SOK-F02      TO  DSP-BASHNM (IX-GYO)
     ELSE
         MOVE  ALL NC"＊"   TO  DSP-BASHNM (IX-GYO)
     END-IF.

* 商品名称マスタ
     MOVE  TB-SHOCD  (CT-PAGE IX-GYO)  TO  MEI-F011. *> 商品CD
     MOVE  TB-HINTAN (CT-PAGE IX-GYO)  TO  MEI-F012. *> 品単
     PERFORM  RD-HMEIMS-SEC.
     IF  FG-HMEIMS-INV = ZERO
         MOVE  MEI-F02      TO  DSP-SHONM (IX-GYO)
     ELSE
         MOVE  ALL NC"＊"   TO  DSP-SHONM (IX-GYO)
     END-IF.

 GMN-SETB-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 GMN-SETB-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ検索                                            *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-ZSOKMS-SEC"  TO  S-NAME.

     READ  ZSOKMS
       INVALID
         MOVE  1            TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-ZSOKMS-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 RD-ZSOKMS-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ検索                                        *
****************************************************************
 RD-HMEIMS-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "RD-HMEIMS-SEC"  TO  S-NAME.

     READ  HMEIMS
       INVALID
         MOVE  1            TO  FG-HMEIMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HMEIMS-INV
     END-READ.

     MOVE  S-NAME-SV        TO  S-NAME.
 RD-HMEIMS-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "MAIN-SEC"       TO  S-NAME.

     EVALUATE  PSW
*    メイン
       WHEN  "1"  PERFORM  DSP-BODY-SEC

       WHEN  OTHER  CONTINUE
     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  明細項目  ( PSW = 1 )                                       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-BODY-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         IF  CT-SEQ > ZERO
             CONTINUE
         ELSE
             IF  ERR-FLG = ZERO
                 MOVE  5    TO  ERR-FLG
             END-IF
         END-IF

*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
*I*******MOVE  "4010"       TO  PROGRAM-STATUS

*    前頁
       WHEN  "F011"
         PERFORM  PG-BACKWD-SEC

*    次頁
       WHEN  "F012"
         PERFORM  PG-FORWD-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
         GO TO  DSP-BODY-SEC

     END-EVALUATE.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*  画面表示処理                                                *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
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
*  ガイドメッセージの設定（次入力モードに切換後）
     EVALUATE  PSW
*    メイン入力
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE        TO  DSP-PFGAID

     END-EVALUATE.

*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FNA00801"       TO  DSP-FMT.
     WRITE  DSP-FNA00801.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE  PSW
*    メイン
       WHEN  "1"
         MOVE  "KAKU"       TO  DSP-GRP

     END-EVALUATE.

     MOVE  "FNA00801"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
     MOVE  SPACE            TO   DSP-PRO.

     MOVE  S-NAME-SV        TO  S-NAME.
 DSP-READ-EXIT.
     EXIT.

****************************************************************
*  前頁画面編集                                                *
****************************************************************
 PG-BACKWD-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE "PG-BACKWD-SEC"   TO  S-NAME.

     IF  CT-PAGE = 1
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         GO TO  PG-BACKWD-090
     END-IF.

     COMPUTE  CT-PAGE = CT-PAGE - 1.
     PERFORM  GMN-SET-SEC.

 PG-BACKWD-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PG-BACKWD-EXIT.
     EXIT.
****************************************************************
*  次頁画面編集                                                *
****************************************************************
 PG-FORWD-SEC         SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE "PG-FORWD-SEC"    TO  S-NAME.

     IF  CT-PAGE = DSP-GPG *> 最終ページ
         EVALUATE  TRUE
            WHEN  CT-PAGE < 99        *> 次頁なし
              IF  ERR-FLG = ZERO
                  MOVE  3   TO  ERR-FLG
              END-IF

            WHEN  FG-TBL-MAXOVER = 1  *> データあり次頁なし
              IF  ERR-FLG = ZERO
                  MOVE  4   TO  ERR-FLG
              END-IF

            WHEN  OTHER                *> データなし次頁なし
              IF  ERR-FLG = ZERO
                  MOVE  3   TO  ERR-FLG
              END-IF
         END-EVALUATE

         GO TO  PG-FORWD-090
     END-IF.

     COMPUTE  CT-PAGE = CT-PAGE + 1.
     PERFORM  GMN-SET-SEC.

 PG-FORWD-090.
     MOVE  S-NAME-SV        TO  S-NAME.
 PG-FORWD-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  S-NAME           TO  S-NAME-SV.
     MOVE  "END-SEC"        TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  NARRUIF.
     CLOSE  NARKANF.
     CLOSE  HTOKMS.
     CLOSE  HTANMS.
     CLOSE  HMEIMS.
     CLOSE  ZSOKMS.
     CLOSE  DSPFILE.

     MOVE  S-NAME-SV        TO  S-NAME.
*
 END-EXIT.
     EXIT.
*****************<<  SNA0080A   END PROGRAM  >>******************

```
