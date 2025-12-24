# SBT0090A

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0090A.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：出荷連携サブシステム　　　
*    業務名　　　　　：
*    モジュール名　　：オンライン連携状況照会
*    作成日／更新日　：2012/10/03
*    作成者／更新者　：三浦/NAV
*    処理概要　　　　：
*      出荷連携したｵﾝﾗｲﾝ発注ﾃﾞｰﾀの状態を照会・発行する
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBT0090A.
 AUTHOR.               M.T.
 DATE-WRITTEN.         2012/10/03.
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

*連携オンライン状況ファイル
     SELECT  LNKONLF
       ASSIGN    TO    LNKONLL2
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
         LNK-F04  *> 倉庫コード
         LNK-F01  *> バッチＮＯ（日付）
         LNK-F02  *> バッチＮＯ（時刻）
         LNK-F03  *> バッチＮＯ（取引先ＣＤ）
         LNK-F05  *> 納品日
       FILE STATUS     LNK-ST.

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
*倉庫マスタ
     SELECT  ZSOKMS
       ASSIGN    TO    ZSOKMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         SOK-F01 *> 倉庫ＣＤ
       FILE STATUS     SOK-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
     LABEL     RECORD    IS   OMITTED.
     COPY      FBT00901  OF   XMDLIB
     JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 連携オンライン状況ファイル                         *
****************************************************************
 FD  LNKONLF
     LABEL     RECORD    IS   STANDARD.
     COPY      LNKONLF   OF   XFDLIB
     JOINING   LNK       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ
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
*    FILE = 倉庫マスタ                                       *
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
     03  LNK-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  TAN-ST             PIC  X(02).
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
     03  SET-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-LNKONLF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HTANMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-ZSOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-TBL-MAXOVER     PIC  9(01).
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
     03  CT-SEQ             PIC  9(03).
     03  CT-PAGE            PIC  9(04).
     03  IX                 PIC  9(04).
     03  IX-TBL             PIC  9(04).
     03  IX-GYO             PIC  9(03).
     03  CT-SEL             PIC  9(03).
     03  FG-SEL             PIC  9(01).
     03  WK-SELGYO          PIC  9(02).
     03  WK-TORNMW          PIC  N(10).
     03  WK-TANNMW          PIC  N(05).
     03  WK-BTNO.
       05  WK-BTNO-NEN      PIC  9(04).
       05  FILLER           PIC  X(01)  VALUE  "/".
       05  WK-BTNO-TUKI     PIC  9(02).
       05  FILLER           PIC  X(01)  VALUE  "/".
       05  WK-BTNO-HI       PIC  9(02).
       05  FILLER           PIC  X(01)  VALUE  "-".
       05  WK-BTNO-HH       PIC  9(02).
       05  FILLER           PIC  X(01)  VALUE  ":".
       05  WK-BTNO-MM       PIC  9(02).
       05  FILLER           PIC  X(01)  VALUE  "-".
       05  WK-BTNO-TORCD    PIC  9(08).
     03  WK-NOHNBIW.
       05  WK-NOHNBI-YY     PIC  9(04).
       05  FILLER           PIC  X(01)  VALUE  "/".
       05  WK-NOHNBI-MM     PIC  9(02).
       05  FILLER           PIC  X(01)  VALUE  "/".
       05  WK-NOHNBI-DD     PIC  9(02).
     03  WK-RNKBI.
       05  WK-RNKBI-YY      PIC  9(04).
       05  FILLER           PIC  X(01)  VALUE  "/".
       05  WK-RNKBI-MM      PIC  9(02).
       05  FILLER           PIC  X(01)  VALUE  "/".
       05  WK-RNKBI-DD      PIC  9(02).
     03  WK-RNKSHA.
       05  WK-RNKSHA-BMN    PIC  X(04).
       05  FILLER           PIC  X(01)  VALUE  "-".
       05  WK-RNKSHA-TAN    PIC  X(02).

* 画面表示データ格納領域
 01  TB-DT.
     03  TB-DT-G.
       05  TB-DT-G2  OCCURS 50. *> ページ
         07  TB-DT-G2  OCCURS 7. *> 行
           09  TB-BTNO      PIC  X(25).
           09  TB-BTTORN    PIC  N(10).
           09  TB-NOHNBI    PIC  X(10).
           09  TB-JKENSU    PIC  9(07).
           09  TB-DENMSU    PIC  9(07).
           09  TB-RNKZSU    PIC  9(07).
           09  TB-MIRNKS    PIC  9(07).
           09  TB-RNKBI     PIC  X(10).
           09  TB-RNKSHA    PIC  X(07).
           09  TB-RNKNM     PIC  N(05).
     03  TB-DT-GR  REDEFINES TB-DT-G.
       05  TB-DT-G2R  OCCURS 350.
           07  TB-BTNO-R    PIC  X(25).
           07  TB-BTTORN-R  PIC  N(10).
           07  TB-NOHNBI-R  PIC  X(10).
           07  TB-JKENSU-R  PIC  9(07).
           07  TB-DENMSU-R  PIC  9(07).
           07  TB-RNKZSU-R  PIC  9(07).
           07  TB-MIRNKS-R  PIC  9(07).
           07  TB-RNKBI-R   PIC  X(10).
           07  TB-RNKSHA-R  PIC  X(07).
           07  TB-RNKNM-R   PIC  N(05).

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
       VALUE NC"_取消　_終了".
     03  PF-MSG2.
       05  FILLER           PIC  N(30)
       VALUE NC"_取消　_終了　_項目戻り".
     03  PF-MSG3.
       05  FILLER           PIC  N(30)
       VALUE NC"_取消　_終了　_項目戻り　_前頁　_次頁".
 01  PF-MSG-AREA-R  REDEFINES PF-MSG-AREA.
     03  PF-MSG-R           PIC  N(30)  OCCURS 3.

*メッセージの取得
*A---B-------2---------3---------4---------5---------6---------7E*
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER         PIC  N(30)  VALUE
         NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER         PIC  N(30)  VALUE
         NC"照会倉庫が未入力です。入力して下さい。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
           NC"入力倉庫は倉庫マスタに存在しません。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
         NC"１、２、３以外の入力は無効です。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
         NC"入力された取引先は取引先マスタに存在しません。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
         NC"指定された条件での表示対象データはありません。".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
         NC"基準受信日が入力されていません。".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
         NC"基準受信日は日付を入力して下さい。".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
         NC"１、２以外の入力は無効です。".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
         NC"　".
     03  ERR-MSG11.
         05  FILLER         PIC  N(30)  VALUE
         NC"前ページはありません。".
     03  ERR-MSG12.
         05  FILLER         PIC  N(30)  VALUE
         NC"次ページはありません。".
     03  ERR-MSG13.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"指定条件に誤りがないか確認し、ＥＮＴＥＲを押下して下
-          "さい。".
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
     03  LNK-ERR           PIC  N(20)  VALUE
         NC"連携オンライン状況ファイルエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  TAN-ERR           PIC  N(20)  VALUE
         NC"担当者マスタエラー".
     03  SOK-ERR           PIC  N(20)  VALUE
         NC"倉庫マスタエラー".

*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 COPY      FNA00601  OF   XMDLIB
 JOINING   WK        AS   PREFIX.
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
 01  PAR-SOKO-IN           PIC  X(02).
 01  PAR-DSOKO-IN          PIC  X(02).

* 出力パラメータ
*    パラＡ
 01  PAR-CHKBN              PIC  X(01).
 01  PAR-SSOKO              PIC  X(02).
 01  PAR-SHOKBN             PIC  X(01).
 01  PAR-KJNJBI             PIC  X(08).
 01  PAR-TORCD              PIC  X(08).
*
**************************************************************
 PROCEDURE             DIVISION
                           USING  PAR-SOKO-IN
                                  PAR-DSOKO-IN
                                  PAR-CHKBN
                                  PAR-SSOKO
                                  PAR-SHOKBN
                                  PAR-KJNJBI
                                  PAR-TORCD.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 LNK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKONLF.
     DISPLAY     LNK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     LNK-ST    UPON      CONS.
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
     OPEN  INPUT LNKONLF.
     OPEN  INPUT HTOKMS.
     OPEN  INPUT HTANMS.
     OPEN  INPUT ZSOKMS.
*ワークの初期化
     INITIALIZE  FLG-AREA.
     MOVE  ZERO             TO  PSW.
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
*    ヘッダ入力
       WHEN  "1"  PERFORM  DSP-HEAD-SEC
*    確認入力
       WHEN  "9"  PERFORM  DSP-KAKU-SEC

       WHEN  OTHER  CONTINUE
     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  ヘッダ入力処理  ( PSW = 1 )                                 *
****************************************************************
 DSP-HEAD-SEC         SECTION.
     MOVE  "DSP-HEAD-SEC"  TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  HEAD-CHK-SEC

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

 DSP-HEAD-EXIT.
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
*  ガイドメッセージの設定（次入力モードに切換後）
     EVALUATE  PSW
*    ヘッダ入力
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-PFGAID
*    確認入力
       WHEN  "9"
         IF DSP-GPG > 1
            MOVE  PF-MSG-R (3) TO  DSP-PFGAID
         ELSE
            MOVE  PF-MSG-R (2) TO  DSP-PFGAID
         END-IF
       WHEN  OTHER
         MOVE  SPACE        TO  DSP-PFGAID

     END-EVALUATE.

*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FBT00901"       TO  DSP-FMT.
     WRITE  DSP-FBT00901.

 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.

*  項目クリア
     MOVE  SPACE         TO  DSP-FBT00901

*  リバース，カーソルパーク解除
     MOVE  "M"          TO  EDIT-OPTION OF DSP-SSOKO.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SSOKO.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-SHOKBN.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-SHOKBN.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-KJNJBI.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-KJNJBI.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-CHKBN.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-CHKBN.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-KAKUNI.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-KAKUNI.
*
*  ＰＧＩＤ
     MOVE  "SBT0090A"       TO  DSP-PGID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*    帳票区分
     MOVE  "1"          TO  DSP-CHKBN.
*    照会倉庫
     PERFORM  PAR-SOKO-CHK-SEC.

 INT-DSP-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE  PSW
*    ヘッダ入力
       WHEN  "1"
         MOVE  "GRP001"     TO  DSP-GRP
*    確認入力
       WHEN  "9"
         MOVE  "KAKU"       TO  DSP-GRP

     END-EVALUATE.

     MOVE  "FBT00901"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
     MOVE  SPACE            TO   DSP-PRO.

 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  パラメータ倉庫入力チェック
****************************************************************
 PAR-SOKO-CHK-SEC         SECTION.
     MOVE  "PAR-SOKO-CHK-SEC"  TO  S-NAME.
     IF      PAR-DSOKO-IN  =  "99"
         MOVE    PAR-SOKO-IN  TO   SOK-F01   DSP-SSOKO
         READ      ZSOKMS
             INVALID  KEY
                 MOVE    SPACE     TO   DSP-SOKONM
             NOT INVALID  KEY
                 MOVE    SOK-F02   TO   DSP-SOKONM
         END-READ
         MOVE    "X"    TO   EDIT-STATUS OF DSP-SSOKO
     ELSE
         MOVE    SPACE  TO  DSP-SSOKO   DSP-SOKONM
         MOVE    "M"    TO  EDIT-OPTION  OF  DSP-SSOKO
         MOVE    SPACE  TO  EDIT-CURSOR  OF  DSP-SSOKO
     END-IF.
****************************************************************
*  ヘッダチェック                                              *
****************************************************************
 HEAD-CHK-SEC          SECTION.
     MOVE  "HEAD-CHK-SEC"  TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-SSOKO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SSOKO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SHOKBN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHOKBN.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORCD.
     MOVE  SPACE            TO  DSP-TORNM.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KJNJBI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KJNJBI.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-CHKBN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHKBN.

* 倉庫ＣＤ
     IF  DSP-SSOKO = SPACE
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SSOKO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SSOKO
         GO TO  HEAD-CHK-EXIT
     END-IF.

     MOVE  DSP-SSOKO        TO  SOK-F01. *> 倉庫ＣＤ
     PERFORM  RD-ZSOKMS-SEC.
     IF  FG-ZSOKMS-INV = ZERO
         MOVE  SOK-F02      TO  DSP-SOKONM
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SSOKO
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SSOKO
         GO TO  HEAD-CHK-EXIT
     END-IF.

* 照会区分
     IF  DSP-SHOKBN = "1" OR "2" OR "3"
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SHOKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SHOKBN
         GO TO  HEAD-CHK-EXIT
     END-IF.
* 基準受信日
***  基準受信日未入力チェック
     IF  DSP-KJNJBI   NOT NUMERIC
         OR   DSP-KJNJBI  =  ZERO
              MOVE   7       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KJNJBI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KJNJBI
              GO TO  HEAD-CHK-EXIT
     ELSE
***  基準受信日論理チェック
         MOVE     "2"            TO   LINK-IN-KBN
         MOVE     ZERO           TO   LINK-IN-YMD6
         MOVE     DSP-KJNJBI     TO   LINK-IN-YMD8
         MOVE     ZERO           TO   LINK-OUT-RET
         MOVE     ZERO           TO   LINK-OUT-YMD
         CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF   LINK-OUT-RET   = 9
              MOVE  8        TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF DSP-KJNJBI
              MOVE  "C"      TO   EDIT-CURSOR  OF DSP-KJNJBI
              GO TO  HEAD-CHK-EXIT
         END-IF
*
     END-IF.

* 取引先ＣＤ
     IF  DSP-TORCD = SPACE
         CONTINUE
     ELSE
         IF  DSP-TORCD = ZERO
             MOVE  SPACE      TO  DSP-TORCD(1:8)
             MOVE  SPACE      TO  DSP-TORNM
         ELSE
             MOVE  DSP-TORCD     TO  TOK-F01
             PERFORM  RD-HTOKMS-SEC
             IF  FG-HTOKMS-INV = ZERO
                 MOVE  TOK-F02      TO  DSP-TORNM
             ELSE
                 IF  ERR-FLG = ZERO
                     MOVE  5        TO  ERR-FLG
                 END-IF
                 MOVE  "C"    TO  EDIT-CURSOR OF DSP-TORCD
                 MOVE  "R"    TO  EDIT-OPTION OF DSP-TORCD
                 GO TO  HEAD-CHK-EXIT
             END-IF
         END-IF
     END-IF.
* 帳票区分
     IF  DSP-CHKBN = "1" OR "2"
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-CHKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-CHKBN
         GO TO  HEAD-CHK-EXIT
     END-IF.

     INITIALIZE  LNK-REC.
     MOVE  DSP-SSOKO        TO  LNK-F04.  *> 倉庫ＣＤ
     MOVE  DSP-KJNJBI       TO  LNK-F01.  *> バッチ日付
     MOVE  ZERO             TO  LNK-F02.  *> バッチ時刻
     IF  DSP-TORCD = SPACE  OR ZERO
         MOVE  ZERO         TO  LNK-F03
     ELSE
         MOVE  DSP-TORCD    TO  LNK-F03
     END-IF.
     MOVE  ZERO             TO  LNK-F05.  *> 納品日

     MOVE  LOW-VALUE        TO  FG-LNKONLF-END.
     PERFORM  RD-LNKONLF-SEC.
     IF  FG-LNKONLF-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-CHKBN
         MOVE  "R"          TO  EDIT-OPTION OF DSP-CHKBN
         GO TO  HEAD-CHK-EXIT
     END-IF.
* 照会データＴＢＬ設定
     PERFORM  TBL-SET-SEC.

* 先頭ページ表示
     IF  DSP-CHKBN = "1"
         MOVE  1           TO  CT-PAGE
         PERFORM  GMN-SET-SEC
     ELSE
         MOVE  13    TO   ERR-FLG
     END-IF.
*チェックＯＫ
*    IF  ERR-FLG NOT = ZERO
*        GO TO  HEAD-CHK-EXIT
*    END-IF.

     MOVE  9             TO  PSW.

 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ検索                                          *
****************************************************************
 RD-HTANMS-SEC          SECTION.
     MOVE  "RD-HTANMS-SEC"  TO  S-NAME.

     READ  HTANMS
       INVALID
         MOVE  1                 TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTANMS-INV
     END-READ.

 RD-HTANMS-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ検索                                          *
****************************************************************
 RD-HTOKMS-SEC          SECTION.
     MOVE  "RD-HTOKMS-SEC"  TO  S-NAME.

     READ  HTOKMS
       INVALID
         MOVE  1                 TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTOKMS-INV
     END-READ.

 RD-HTOKMS-EXIT.
     EXIT.

****************************************************************
*    倉庫マスタ検索                                          *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     MOVE  "RD-HSOKMS-SEC"  TO  S-NAME.

     READ  ZSOKMS
       INVALID
         MOVE  1                 TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-ZSOKMS-INV
     END-READ.

 RD-ZSOKMS-EXIT.
     EXIT.
****************************************************************
*    連携オンライン状況ファイル読込み　
****************************************************************
 RD-LNKONLF-SEC        SECTION.
     MOVE  "RD-LNKONLF-SEC" TO  S-NAME.

     MOVE SPACE  TO  SET-FLG.
     IF  FG-LNKONLF-END = LOW-VALUE
         START  LNKONLF  KEY >=  LNK-F04   *> 倉庫ＣＤ
                                 LNK-F01   *> バッチ日付
                                 LNK-F02   *> バッチ時刻
                                 LNK-F03   *> バッチ取引先
                                 LNK-F05   *> 納品日
           INVALID KEY
              MOVE  "END"   TO  FG-LNKONLF-END
              GO TO  RD-LNKONLF-EXIT
         END-START

         MOVE  SPACE        TO  FG-LNKONLF-END

     END-IF.

     READ  LNKONLF
       AT  END
         MOVE  "END"        TO  FG-LNKONLF-END
         GO TO  RD-LNKONLF-EXIT
     END-READ.

* 倉庫ＣＤ
     IF  LNK-F04 = DSP-SSOKO
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-LNKONLF-END
         GO TO  RD-LNKONLF-EXIT
     END-IF.
* 取引先ＣＤ
     IF  DSP-TORCD NOT = SPACE  AND DSP-TORCD NOT = ZERO
         IF  LNK-F03 = DSP-TORCD
             CONTINUE
         ELSE
*            MOVE  "END"    TO  FG-LNKONLF-END
             GO TO  RD-LNKONLF-EXIT
         END-IF
     END-IF.
* バッチ日付
     IF   LNK-F01 >= DSP-KJNJBI
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-LNKONLF-END
         GO TO  RD-LNKONLF-EXIT
     END-IF.
* 照会区分
     IF  DSP-SHOKBN = "2"
         IF LNK-F09  = 0
             CONTINUE
         ELSE
             GO TO  RD-LNKONLF-EXIT
         END-IF
     END-IF.
     IF  DSP-SHOKBN = "3"
         IF LNK-F09  NOT = 0
             CONTINUE
         ELSE
             GO TO  RD-LNKONLF-EXIT
         END-IF
     END-IF.
     MOVE "SET"  TO  SET-FLG.

 RD-LNKONLF-EXIT.
     EXIT.

****************************************************************
*    表示データ格納ＴＢＬ設定                                  *
****************************************************************
 TBL-SET-SEC           SECTION.
     MOVE  "TBL-SET-SEC"    TO  S-NAME.

     MOVE  SPACE            TO  TB-DT.
     INITIALIZE  TB-DT.
     MOVE  ZERO             TO  CT-SEQ.
     MOVE  ZERO             TO  IX-TBL.
     MOVE  ZERO             TO  FG-TBL-MAXOVER.

     PERFORM  UNTIL FG-LNKONLF-END = "END"
       IF SET-FLG = "SET"
           PERFORM  TBL-SETB-SEC
       END-IF
       PERFORM  RD-LNKONLF-SEC
     END-PERFORM.

 TBL-SET-EXIT.
     EXIT.
****************************************************************
*    表示データ格納ＴＢＬ設定Ｂ                                *
****************************************************************
 TBL-SETB-SEC           SECTION.
     MOVE  "TBL-SETB-SEC"   TO  S-NAME.

     IF  IX-TBL >= 350
         MOVE  1            TO  FG-TBL-MAXOVER
         GO TO  TBL-SETB-EXIT
     END-IF.

     ADD  1   TO  IX-TBL.
     ADD  1   TO  CT-SEQ.
     IF  LNK-F03 NOT = WK-BTNO-TORCD
         MOVE  LNK-F03      TO  TOK-F01
         PERFORM  RD-HTOKMS-SEC
         IF  FG-HTOKMS-INV = ZERO
             MOVE  TOK-F02      TO  WK-TORNMW
         ELSE
             MOVE  SPACE        TO  WK-TORNMW
         END-IF
     END-IF.
     IF (LNK-F11 = WK-RNKSHA-BMN) AND (LNK-F12 = WK-RNKSHA-TAN)
         CONTINUE
     ELSE
         MOVE  LNK-F11     TO  TAN-F01
         MOVE  LNK-F12     TO  TAN-F02
         PERFORM  RD-HTANMS-SEC
         IF  FG-HTANMS-INV = ZERO
             MOVE  TAN-F03      TO  WK-TANNMW
         ELSE
             MOVE  SPACE        TO  WK-TANNMW
         END-IF
     END-IF.
     MOVE  LNK-F01(1:4)     TO  WK-BTNO-NEN.
     MOVE  LNK-F01(5:2)     TO  WK-BTNO-TUKI.
     MOVE  LNK-F01(7:2)     TO  WK-BTNO-HI.
     MOVE  LNK-F02(1:2)     TO  WK-BTNO-HH.
     MOVE  LNK-F02(3:2)     TO  WK-BTNO-MM.
     MOVE  LNK-F03          TO  WK-BTNO-TORCD.
     MOVE  LNK-F05(1:4)     TO  WK-NOHNBI-YY.
     MOVE  LNK-F05(5:2)     TO  WK-NOHNBI-MM.
     MOVE  LNK-F05(7:2)     TO  WK-NOHNBI-DD.
     MOVE  LNK-F10(1:4)     TO  WK-RNKBI-YY.
     MOVE  LNK-F10(5:2)     TO  WK-RNKBI-MM.
     MOVE  LNK-F10(7:2)     TO  WK-RNKBI-DD.
     MOVE  LNK-F11          TO  WK-RNKSHA-BMN.
     MOVE  LNK-F12          TO  WK-RNKSHA-TAN.
     MOVE  WK-BTNO          TO  TB-BTNO-R   (IX-TBL).
     MOVE  WK-TORNMW        TO  TB-BTTORN-R (IX-TBL).
     MOVE  WK-NOHNBIW       TO  TB-NOHNBI-R (IX-TBL).
     MOVE  LNK-F06          TO  TB-JKENSU-R (IX-TBL).
     MOVE  LNK-F07          TO  TB-DENMSU-R (IX-TBL).
     MOVE  LNK-F08          TO  TB-RNKZSU-R (IX-TBL).
     MOVE  LNK-F09          TO  TB-MIRNKS-R (IX-TBL).

     MOVE  WK-RNKBI         TO  TB-RNKBI-R  (IX-TBL).
     MOVE  WK-RNKSHA        TO  TB-RNKSHA-R (IX-TBL).
     MOVE  WK-TANNMW        TO  TB-RNKNM-R (IX-TBL).

 TBL-SETB-EXIT.
     EXIT.
****************************************************************
*    画面編集処理                                              *
****************************************************************
 GMN-SET-SEC           SECTION.
     MOVE  "GMN-SET-SEC"    TO  S-NAME.

     DIVIDE  7  INTO CT-SEQ
       GIVING    WK-SYO
       REMAINDER WK-AMARI.

     MOVE  CT-PAGE          TO  DSP-PG.
     IF WK-AMARI = ZERO
        MOVE  WK-SYO        TO  DSP-GPG
     ELSE
        COMPUTE  DSP-GPG = WK-SYO + 1
     END-IF.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 7
       PERFORM  GMN-SETB-SEC
     END-PERFORM.

 GMN-SET-EXIT.
     EXIT.
****************************************************************
*    画面編集Ｂ処理                                            *
****************************************************************
 GMN-SETB-SEC          SECTION.
     IF      CT-PAGE = DSP-GPG
         AND WK-AMARI NOT = ZERO
         AND IX-GYO > WK-AMARI  *> 最終ページのデータなし行

         MOVE  SPACE        TO  DSP-BTNO   (IX-GYO)
         MOVE  SPACE        TO  DSP-BTTORN (IX-GYO)
         MOVE  SPACE        TO  DSP-NOHNBI (IX-GYO)
         MOVE  SPACE        TO  DSP-JKENSU (IX-GYO)(1:7)
         MOVE  SPACE        TO  DSP-DENMSU (IX-GYO)(1:7)
         MOVE  SPACE        TO  DSP-RNKZSU (IX-GYO)(1:7)
         MOVE  SPACE        TO  DSP-MIRNKS (IX-GYO)(1:7)
         MOVE  SPACE        TO  DSP-RNKBI  (IX-GYO)
         MOVE  SPACE        TO  DSP-RNKSHA (IX-GYO)
         MOVE  SPACE        TO  DSP-RNKNM  (IX-GYO)

         GO TO  GMN-SETB-EXIT
     END-IF.

     MOVE  TB-BTNO   (CT-PAGE IX-GYO)     TO  DSP-BTNO   (IX-GYO).
     MOVE  TB-BTTORN (CT-PAGE IX-GYO)     TO  DSP-BTTORN (IX-GYO).
     MOVE  TB-NOHNBI (CT-PAGE IX-GYO)     TO  DSP-NOHNBI (IX-GYO).
     MOVE  TB-JKENSU (CT-PAGE IX-GYO)     TO  DSP-JKENSU (IX-GYO).
     MOVE  TB-DENMSU (CT-PAGE IX-GYO)     TO  DSP-DENMSU (IX-GYO).
     MOVE  TB-RNKZSU (CT-PAGE IX-GYO)     TO  DSP-RNKZSU (IX-GYO).
     MOVE  TB-MIRNKS (CT-PAGE IX-GYO)     TO  DSP-MIRNKS (IX-GYO).
     MOVE  TB-RNKBI  (CT-PAGE IX-GYO)     TO  DSP-RNKBI  (IX-GYO).
     MOVE  TB-RNKSHA (CT-PAGE IX-GYO)     TO  DSP-RNKSHA (IX-GYO).
     MOVE  TB-RNKNM  (CT-PAGE IX-GYO)     TO  DSP-RNKNM  (IX-GYO).

 GMN-SETB-EXIT.
     EXIT.
****************************************************************
*  前頁画面編集                                                *
****************************************************************
 PG-BACKWD-SEC         SECTION.
     MOVE "PG-BACKWD-SEC"   TO  S-NAME.

     IF  CT-PAGE = 1
         IF  ERR-FLG = ZERO
             MOVE  11       TO  ERR-FLG
         END-IF
         GO TO  PG-BACKWD-EXIT
     END-IF.

     COMPUTE  CT-PAGE = CT-PAGE - 1.
     PERFORM  GMN-SET-SEC.

 PG-BACKWD-EXIT.
     EXIT.
****************************************************************
*  次頁画面編集                                                *
****************************************************************
 PG-FORWD-SEC         SECTION.
     MOVE "PG-FORWD-SEC"    TO  S-NAME.

     IF  CT-PAGE = DSP-GPG *> 最終ページ
         EVALUATE  TRUE
            WHEN  CT-PAGE < 50        *> 次頁なし
              IF  ERR-FLG = ZERO
                  MOVE  12  TO  ERR-FLG
              END-IF

            WHEN  FG-TBL-MAXOVER = 1  *> データあり次頁なし
              IF  ERR-FLG = ZERO
                  MOVE  10  TO  ERR-FLG
              END-IF

            WHEN  OTHER                *> データなし次頁なし
              IF  ERR-FLG = ZERO
                  MOVE  12  TO  ERR-FLG
              END-IF
         END-EVALUATE

         GO TO  PG-FORWD-EXIT
     END-IF.

     COMPUTE  CT-PAGE = CT-PAGE + 1.
     PERFORM  GMN-SET-SEC.

 PG-FORWD-EXIT.
     EXIT.
****************************************************************
*  確認処理入力 （ PSW = 9 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
           IF DSP-CHKBN = "2"
               PERFORM PARA-OT-SEC
               MOVE  "END"        TO  END-FLG
           END-IF
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*    項目戻し
       WHEN  "F006"
         MOVE  "1"          TO  PSW
*    取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC
*    前頁
       WHEN  "F011"
         PERFORM  PG-BACKWD-SEC

*    次頁
       WHEN  "F012"
         PERFORM  PG-FORWD-SEC

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
     MOVE  "PARA-OT-SEC"    TO  S-NAME.

* 帳票区分
     MOVE  DSP-CHKBN            TO  PAR-CHKBN.
* 照会倉庫
     MOVE  DSP-SSOKO            TO  PAR-SSOKO.
* 照会区分
     MOVE  DSP-SHOKBN           TO  PAR-SHOKBN.
* 基準受信日
     MOVE  DSP-KJNJBI           TO  PAR-KJNJBI.
* 取引先ＣＤ
     IF DSP-TORCD = SPACE
         MOVE  "00000000"       TO  PAR-TORCD
     ELSE
         MOVE  DSP-TORCD        TO  PAR-TORCD
     END-IF.

 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  HTOKMS.
     CLOSE  LNKONLF.
     CLOSE  HTANMS.
     CLOSE  ZSOKMS.
     CLOSE  DSPFILE.
*
 END-EXIT.
     EXIT.
*****************<<  SBT0090A   END PROGRAM  >>******************

```
