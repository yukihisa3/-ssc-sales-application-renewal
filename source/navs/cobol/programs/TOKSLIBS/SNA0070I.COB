***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：小売連携発注データ照会選択（手書）
*    作成日／更新日　：2011/11/09
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      小売連携した手書発注ﾃﾞｰﾀを照会するため、
*    （苗側取込み済・未済や取り消し済・未済など）
*      連携Ｎｏを画面より選択する。
*      選択されたＮｏはパラメータにより照会（詳細）
*      へ引き渡す。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0070I.
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

*連携Ｎｏ管理テーブル
     SELECT  NARKANF
       ASSIGN    TO    NARKANL2
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
*I*******KAN-F071 *> 入力区分
*        KAN-F03  *> 担当者ＣＤ
*        KAN-F04  *> 状態ＦＬＧ
*        KAN-F076 *> 取引先ＣＤ
*        KAN-F01  *> 連携Ｎｏ
********************************
         KAN-F071 *> 入力区分
         KAN-F02  *> 部門ＣＤ
         KAN-F03  *> 担当者ＣＤ
         KAN-F076 *> 取引先ＣＤ
         KAN-F074 *> 納品日
         KAN-F01  *> 連携Ｎｏ
         KAN-F04  *> 状態ＦＬＧ
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

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
     LABEL     RECORD    IS   OMITTED.
     COPY      FNA00701  OF   XMDLIB
     JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 連携Ｎｏ管理テーブル                               *
****************************************************************
 FD  NARKANF
     LABEL     RECORD    IS   STANDARD.
     COPY      NARKANF   OF   XFDLIB
     JOINING   KAN       AS   PREFIX.
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
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST             PIC  X(02).
     03  KAN-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  TAN-ST             PIC  X(02).
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
     03  FG-NARKANF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HTANMS-INV      PIC  9(01)  VALUE  ZERO.
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

* 画面表示データ格納領域
 01  TB-DT.
     03  TB-DT-G.
       05  TB-DT-G2  OCCURS 50. *> ページ
         07  TB-DT-G2  OCCURS 12. *> 行
           09  TB-SELNO     PIC  X(01).
           09  TB-SEQ       PIC  9(03).
           09  TB-RENNO     PIC  X(09).
           09  TB-BASHO     PIC  X(02).
           09  TB-TORCD     PIC  X(08).
           09  TB-SDENNO    PIC  9(09).
           09  TB-KARA      PIC  N(01).
           09  TB-EDENNO    PIC  9(09).
           09  TB-NOHNBI    PIC  9(08).
           09  TB-JNOHBI    PIC  9(08).

           09  TB-STS       PIC  X(01).
           09  TB-SDENNO2   PIC  9(09).
           09  TB-EDENNO2   PIC  9(09).
           09  TB-SDENNO3   PIC  9(09).
           09  TB-EDENNO3   PIC  9(09).
           09  TB-SDENNO4   PIC  9(09).
           09  TB-EDENNO4   PIC  9(09).
           09  TB-SDENNO5   PIC  9(09).
           09  TB-EDENNO5   PIC  9(09).


     03  TB-DT-GR  REDEFINES TB-DT-G.
       05  TB-DT-G2R  OCCURS 600.
         07  TB-SELNO-R     PIC  X(01).
         07  TB-SEQ-R       PIC  9(03).
         07  TB-RENNO-R     PIC  X(09).
         07  TB-BASHO-R     PIC  X(02).
         07  TB-TORCD-R     PIC  X(08).
         07  TB-SDENNO-R    PIC  9(09).
         07  TB-KARA-R      PIC  N(01).
         07  TB-EDENNO-R    PIC  9(09).
         07  TB-NOHNBI-R    PIC  9(08).
         07  TB-JNOHBI-R    PIC  9(08).

         07  TB-STS-R       PIC  X(01).
         07  TB-SDENNO2-R   PIC  9(09).
         07  TB-EDENNO2-R   PIC  9(09).
         07  TB-SDENNO3-R   PIC  9(09).
         07  TB-EDENNO3-R   PIC  9(09).
         07  TB-SDENNO4-R   PIC  9(09).
         07  TB-EDENNO4-R   PIC  9(09).
         07  TB-SDENNO5-R   PIC  9(09).
         07  TB-EDENNO5-R   PIC  9(09).

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
         NC"担当者コードが未入力です。入力して下さい。".
     03  ERR-MSG3.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
           NC"入力された担当者コードは担当者マスタに存在しません
-           "。".
     03  ERR-MSG4.
         05  FILLER         PIC  N(30)  VALUE
         NC"空白及び１、２、３、４、５以外の入力は無効です。".
     03  ERR-MSG5.
         05  FILLER         PIC  N(30)  VALUE
         NC"入力された取引先は取引先マスタに存在しません。".
     03  ERR-MSG6.
         05  FILLER         PIC  N(30)  VALUE
         NC"指定された条件での表示対象データはありません。".
     03  ERR-MSG7.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"１件も選択されていません、照会する行をＹで選択して下
-         "さい。".
     03  ERR-MSG8.
         05  FILLER         PIC  N(30)  VALUE
         NC"複数Ｙ選択されています。１件だけ選択して下さい。".
     03  ERR-MSG9.
         05  FILLER         PIC  N(30)  VALUE
         NC"選択は、”Ｙ”のみ入力可能です。".
     03  ERR-MSG10.
         05  FILLER         PIC  N(30)  VALUE
*A---B-------2---------3---------4---------5---------6---------7E*
         NC"データはありますが、これ以上のスクロールはできません
-          "。".
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
     03  KAN-ERR           PIC  N(20)  VALUE
         NC"連携Ｎｏ管理テーブルエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  TAN-ERR           PIC  N(20)  VALUE
         NC"担当者マスタエラー".

*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 COPY      FNA00701  OF   XMDLIB
 JOINING   WK        AS   PREFIX.
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
     OPEN  INPUT NARKANF.
     OPEN  INPUT HTOKMS.
     OPEN  INPUT HTANMS.
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
*    担当者入力
       WHEN  "1"  PERFORM  DSP-TANCD-SEC
*    状態指定入力
       WHEN  "2"  PERFORM  DSP-STS-SEC
*    取引先指定入力
       WHEN  "3"  PERFORM  DSP-TORCD-SEC
*    メイン入力
       WHEN  "4"  PERFORM  DSP-BODY-SEC
*    確認入力
       WHEN  "5"  PERFORM  DSP-KAKU-SEC

       WHEN  OTHER  CONTINUE
     END-EVALUATE.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  担当者入力処理  ( PSW = 1 )                                 *
****************************************************************
 DSP-TANCD-SEC         SECTION.
     MOVE  "DSP-TANCD-SEC"  TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  TANCD-CHK-SEC

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

 DSP-TANCD-EXIT.
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
*    担当者入力
       WHEN  "1"
         MOVE  PF-MSG-R (1) TO  DSP-PFGAID
*    状態指定入力
       WHEN  "2"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
*    取引先指定入力
       WHEN  "3"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
*    メイン入力
       WHEN  "4"
         MOVE  PF-MSG-R (3) TO  DSP-PFGAID
*    確認入力
       WHEN  "5"
         MOVE  PF-MSG-R (2) TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE        TO  DSP-PFGAID

     END-EVALUATE.

*  画面の表示
     MOVE  "SCREEN"         TO  DSP-GRP.
     MOVE  "FNA00701"       TO  DSP-FMT.
     WRITE  DSP-FNA00701.

 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.

*  項目クリア
*    担当者入力、確認入力
     IF  PSW = "1" OR "5" OR ZERO
        MOVE  SPACE         TO  DSP-FNA00701
     END-IF.

*    状態指定入力
     IF  PSW = "2"
         MOVE  SPACE        TO  DSP-STS
     END-IF.

*    取引先指定入力
     IF  PSW = "2" OR "3"
         MOVE  SPACE        TO  DSP-TORCD (1:8)
         MOVE  SPACE        TO  DSP-TORNM
     END-IF.

*    メイン入力
     IF  PSW = "2" OR "3" OR "4"
         PERFORM  VARYING IX  FROM 1 BY 1
                  UNTIL   IX > 12
           MOVE  SPACE      TO  DSP-SELNO  (IX)
           MOVE  ZERO       TO  DSP-SEQ    (IX)
           MOVE  SPACE      TO  DSP-RENNO  (IX)
           MOVE  SPACE      TO  DSP-BASHO  (IX)
           MOVE  SPACE      TO  DSP-TORCDM (IX)(1:8)
           MOVE  SPACE      TO  DSP-SDENNO (IX)(1:9)
           MOVE  SPACE      TO  DSP-KARA   (IX)
           MOVE  SPACE      TO  DSP-EDENNO (IX)(1:9)
           MOVE  SPACE      TO  DSP-NOHNBI (IX)(1:8)
           MOVE  SPACE      TO  DSP-JNOHBI (IX)(1:8)
         END-PERFORM
     END-IF.

*  リバース，カーソルパーク解除
*I↓
*    EVALUATE  PSW
*    担当者入力
*      WHEN  "1"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-TANCD
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TANCD
*
*    状態指定入力
*      WHEN  "2"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-STS
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-STS
*
*    取引先指定入力
*      WHEN  "3"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD
*
*    メイン入力
*      WHEN  "4"
*        PERFORM  VARYING IX  FROM 1 BY 1
*                 UNTIL   IX > 12
*          MOVE  "M"        TO  EDIT-OPTION OF DSP-SELNO  (IX)
*          MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SELNO  (IX)
*        END-PERFORM
*
*    確認入力
*      WHEN  "5"
*        MOVE  "M"          TO  EDIT-OPTION OF DSP-KAKUNI
*        MOVE  SPACE        TO  EDIT-CURSOR OF DSP-KAKUNI
*
*    END-EVALUATE.
*I***********************************************************
     MOVE  "M"          TO  EDIT-OPTION OF DSP-TANCD.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TANCD.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-STS.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-STS.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-TORCD.
     PERFORM  VARYING IX  FROM 1 BY 1
              UNTIL   IX > 12
           MOVE  "M"        TO  EDIT-OPTION OF DSP-SELNO  (IX)
           MOVE  SPACE      TO  EDIT-CURSOR OF DSP-SELNO  (IX)
           MOVE  "M"        TO  EDIT-OPTION OF DSP-RENNO  (IX)
     END-PERFORM.
     MOVE  "M"          TO  EDIT-OPTION OF DSP-KAKUNI.
     MOVE  SPACE        TO  EDIT-CURSOR OF DSP-KAKUNI.
*I↑*********************************************************
*
*  ＰＧＩＤ
     MOVE  "SNA0070I"       TO  DSP-PGID.
*  システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*  システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.

 INT-DSP-EXIT.
     EXIT.
****************************************************************
*  画面読込処理                                                *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE  "DSP-READ-SEC"   TO  S-NAME.

     MOVE  "NE"             TO  DSP-PRO.

     EVALUATE  PSW
*    担当者入力
       WHEN  "1"
         MOVE  "TANCD"      TO  DSP-GRP
*    状態指定入力
       WHEN  "2"
         MOVE  "STS"        TO  DSP-GRP
*    取引先指定入力
       WHEN  "3"
         MOVE  "TORCD"      TO  DSP-GRP
*    メイン入力
       WHEN  "4"
         MOVE  "MAIN"       TO  DSP-GRP
*    確認入力
       WHEN  "5"
         MOVE  "KAKU"       TO  DSP-GRP

     END-EVALUATE.

     MOVE  "FNA00701"       TO  DSP-FMT.
     READ  DSPFILE.
*  入力項目の属性を通常にする
     MOVE  SPACE            TO   DSP-PRO.

 DSP-READ-EXIT.
     EXIT.
****************************************************************
*  担当者チェック                                              *
****************************************************************
 TANCD-CHK-SEC          SECTION.
     MOVE  "TANCD-CHK-SEC"  TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-TANCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANCD.
     MOVE  SPACE            TO  DSP-TANNM.

* 担当者ＣＤ
     IF      DSP-TANCD = SPACE
         IF  ERR-FLG = ZERO
             MOVE  2        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TANCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TANCD
         GO TO  TANCD-CHK-EXIT
     END-IF.

     MOVE  PAR-BMNCD-IN     TO  TAN-F01. *> 部門ＣＤ
     MOVE  DSP-TANCD        TO  TAN-F02. *> 担当者ＣＤ
     PERFORM  RD-HTANMS-SEC.
     IF  FG-HTANMS-INV = ZERO
         MOVE  TAN-F03      TO  DSP-TANNM
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TANCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TANCD
         GO TO  TANCD-CHK-EXIT
     END-IF.

*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  TANCD-CHK-EXIT
     END-IF.

     MOVE  2             TO  PSW.

 TANCD-CHK-EXIT.
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
*  状態指定入力処理  ( PSW = 2 )                               *
****************************************************************
 DSP-STS-SEC           SECTION.
     MOVE  "DSP-STS-SEC"    TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  STS-CHK-SEC

*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS

*      項目戻し
       WHEN  "F006"
         MOVE  "1"          TO  PSW

*      取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.

 DSP-STS-EXIT.
     EXIT.
****************************************************************
*  状態指定チェック                                            *
****************************************************************
 STS-CHK-SEC           SECTION.
     MOVE  "STS-CHK-SEC"    TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-STS.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-STS.

* 状態指定
     IF  DSP-STS = SPACE OR "1" OR "2" OR "3" OR "4" OR "5"
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  4        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-STS
         MOVE  "R"          TO  EDIT-OPTION OF DSP-STS
         GO TO  STS-CHK-EXIT
     END-IF.

*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  STS-CHK-EXIT
     END-IF.

     MOVE  3             TO  PSW.

 STS-CHK-EXIT.
     EXIT.
****************************************************************
*  取引先入力処理  ( PSW = 3 )                               *
****************************************************************
 DSP-TORCD-SEC         SECTION.
     MOVE  "DSP-TORCD-SEC"  TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*      実行
       WHEN  "E000"
         PERFORM  TORCD-CHK-SEC

*      終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS

*      項目戻し
       WHEN  "F006"
         MOVE  "2"          TO  PSW

*      取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC
*I****   MOVE  "2"          TO  PSW

       WHEN  OTHER
         MOVE  1            TO  ERR-FLG
     END-EVALUATE.

 DSP-TORCD-EXIT.
     EXIT.
****************************************************************
*  取引先入力チェック                                          *
****************************************************************
 TORCD-CHK-SEC         SECTION.
     MOVE  "TORCD-CHK-SEC"  TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORCD.
     MOVE  SPACE            TO  DSP-TORNM.

* 取引先ＣＤ
     MOVE  DSP-TORCD        TO  TOK-F01 *> 相手取引先コード
     PERFORM  RD-HTOKMS-SEC.
     IF  FG-HTOKMS-INV = ZERO
         MOVE  TOK-F02      TO  DSP-TORNM
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  5        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TORCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TORCD
         GO TO  TORCD-CHK-EXIT
     END-IF.

     INITIALIZE  KAN-REC.
     MOVE  "1"              TO  KAN-F071. *> 入力区分
*I***MOVE  DSP-TANCD        TO  KAN-F03.  *> 担当者ＣＤ
*    MOVE  DSP-STS          TO  KAN-F04.  *> 状態ＦＬＧ
*    MOVE  DSP-TORCD        TO  KAN-F076. *> 取引先ＣＤ
*******************************************************
     MOVE  PAR-BMNCD-IN     TO  KAN-F02.  *> 部門ＣＤ
     MOVE  DSP-TANCD        TO  KAN-F03.  *> 担当者ＣＤ
     MOVE  DSP-TORCD        TO  KAN-F076. *> 取引先ＣＤ
     MOVE  ZERO             TO  KAN-F074. *> 納品日
     MOVE  SPACE            TO  KAN-F01.  *> 連携_
     MOVE  DSP-STS          TO  KAN-F04.  *> 状態ＦＬＧ
*
     MOVE  LOW-VALUE        TO  FG-NARKANF-END.
     PERFORM  RD-NARKANF-SEC.
     IF  FG-NARKANF-END = "END"
         IF  ERR-FLG = ZERO
             MOVE  6        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TORCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-TORCD
         GO TO  TORCD-CHK-EXIT
     END-IF.
* 照会データＴＢＬ設定
     PERFORM  TBL-SET-SEC.

* 先頭ページ表示
     MOVE  1                TO  CT-PAGE
     PERFORM  GMN-SET-SEC.

*チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  TORCD-CHK-EXIT
     END-IF.

     MOVE  4                TO  PSW.

 TORCD-CHK-EXIT.
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
*    連携Ｎｏ管理テーブル読込み　                              *
****************************************************************
 RD-NARKANF-SEC        SECTION.
     MOVE  "RD-NARKANF-SEC" TO  S-NAME.

     IF  FG-NARKANF-END = LOW-VALUE
*I****** START  NARKANF  KEY >=  KAN-F071  *> 入力区分
*                                KAN-F03   *> 担当者ＣＤ
*                                KAN-F04   *> 状態ＦＬＧ
*                                KAN-F076  *> 取引先ＣＤ
********************************************************
         START  NARKANF  KEY >=  KAN-F071  *> 入力区分
                                 KAN-F02   *> 部門ＣＤ
                                 KAN-F03   *> 担当者ＣＤ
                                 KAN-F076  *> 取引先ＣＤ
                                 KAN-F074  *> 納品日
                                 KAN-F01   *> 連携_
                                 KAN-F04   *> 状態ＦＬＧ
           INVALID KEY
              MOVE  "END"   TO  FG-NARKANF-END
              GO TO  RD-NARKANF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NARKANF-END

     END-IF.

     READ  NARKANF
       AT  END
         MOVE  "END"        TO  FG-NARKANF-END
         GO TO  RD-NARKANF-EXIT
     END-READ.

* 入力区分
     IF  KAN-F071 = "1" *> 手書
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-NARKANF-END
         GO TO  RD-NARKANF-EXIT
     END-IF.
* 担当者ＣＤ
*I***IF  KAN-F03 = DSP-TANCD
****************************
     IF  ( KAN-F03 = DSP-TANCD )  AND ( KAN-F02 = PAR-BMNCD-IN)
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-NARKANF-END
         GO TO  RD-NARKANF-EXIT
     END-IF.

     IF  DSP-STS = SPACE
         *> 状態ＦＬＧ

         *> 取引先ＣＤ
         IF  KAN-F076 = DSP-TORCD
             CONTINUE
         ELSE
             MOVE  "END"    TO  FG-NARKANF-END
             GO TO  RD-NARKANF-SEC
         END-IF
     ELSE
         *> 状態ＦＬＧ
         IF KAN-F04 NOT = DSP-STS
*I**********DISPLAY "PASS 2"  UPON CONS
*I**********MOVE  "END"     TO  FG-NARKANF-END
*I**********GO TO  RD-NARKANF-EXIT
            GO TO  RD-NARKANF-SEC
         END-IF
         *> 取引先ＣＤ
         IF  KAN-F076 = DSP-TORCD
             CONTINUE
         ELSE
             MOVE  "END"    TO  FG-NARKANF-END
             GO TO  RD-NARKANF-EXIT
         END-IF
     END-IF.
*
 RD-NARKANF-EXIT.
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

     PERFORM  UNTIL FG-NARKANF-END = "END"
       PERFORM  TBL-SETB-SEC
       PERFORM  RD-NARKANF-SEC
     END-PERFORM.

 TBL-SET-EXIT.
     EXIT.
****************************************************************
*    表示データ格納ＴＢＬ設定Ｂ                                *
****************************************************************
 TBL-SETB-SEC           SECTION.
     MOVE  "TBL-SETB-SEC"   TO  S-NAME.

     IF  IX-TBL >= 600
         MOVE  1            TO  FG-TBL-MAXOVER
         GO TO  TBL-SETB-EXIT
     END-IF.

     ADD  1   TO  IX-TBL.
     ADD  1   TO  CT-SEQ.

     MOVE  CT-SEQ           TO  TB-SEQ-R    (IX-TBL).
     MOVE  KAN-F01          TO  TB-RENNO-R  (IX-TBL).
     MOVE  KAN-F073         TO  TB-BASHO-R  (IX-TBL).
     MOVE  KAN-F076         TO  TB-TORCD-R  (IX-TBL).
     MOVE  KAN-F077         TO  TB-SDENNO-R (IX-TBL).
     MOVE  NC"～"           TO  TB-KARA-R   (IX-TBL).
     MOVE  KAN-F078         TO  TB-EDENNO-R (IX-TBL).
     MOVE  KAN-F074         TO  TB-NOHNBI-R (IX-TBL).
     MOVE  KAN-F075         TO  TB-JNOHBI-R (IX-TBL).

     MOVE  KAN-F04          TO  TB-STS-R    (IX-TBL).
     MOVE  KAN-F079         TO  TB-SDENNO2-R (IX-TBL).
     MOVE  KAN-F07A         TO  TB-EDENNO2-R (IX-TBL).
     MOVE  KAN-F07B         TO  TB-SDENNO3-R (IX-TBL).
     MOVE  KAN-F07C         TO  TB-EDENNO3-R (IX-TBL).
     MOVE  KAN-F07D         TO  TB-SDENNO4-R (IX-TBL).
     MOVE  KAN-F07E         TO  TB-EDENNO4-R (IX-TBL).
     MOVE  KAN-F07F         TO  TB-SDENNO5-R (IX-TBL).
     MOVE  KAN-F07G         TO  TB-EDENNO5-R (IX-TBL).

 TBL-SETB-EXIT.
     EXIT.
****************************************************************
*    画面編集処理                                              *
****************************************************************
 GMN-SET-SEC           SECTION.
     MOVE  "GMN-SET-SEC"    TO  S-NAME.

     DIVIDE  12  INTO CT-SEQ
       GIVING    WK-SYO
       REMAINDER WK-AMARI.

     MOVE  CT-PAGE          TO  DSP-PG.
     IF WK-AMARI = ZERO
        MOVE  WK-SYO        TO  DSP-GPG
     ELSE
        COMPUTE  DSP-GPG = WK-SYO + 1
     END-IF.

     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO > 12
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

*I↓
         MOVE  SPACE        TO  DSP-SELNO  (IX-GYO)
*I↑
         MOVE  ZERO         TO  DSP-SEQ    (IX-GYO)
         MOVE  SPACE        TO  DSP-RENNO  (IX-GYO)
         MOVE  SPACE        TO  DSP-BASHO  (IX-GYO)
         MOVE  SPACE        TO  DSP-TORCDM (IX-GYO)(1:8)
         MOVE  SPACE        TO  DSP-SDENNO (IX-GYO)(1:9)
         MOVE  SPACE         TO  DSP-KARA   (IX-GYO)
         MOVE  SPACE         TO  DSP-EDENNO (IX-GYO)(1:9)
         MOVE  SPACE         TO  DSP-NOHNBI (IX-GYO)(1:8)
         MOVE  SPACE         TO  DSP-JNOHBI (IX-GYO)(1:8)

      *> プロテクトをかける。
         MOVE  "X"  TO  EDIT-STATUS OF DSP-SELNO (IX-GYO)
      *> 色を標準にする。
         MOVE  "M"  TO  EDIT-COLOR OF DSP-RENNO  (IX-GYO)
         MOVE  "M"  TO  EDIT-OPTION OF DSP-RENNO  (IX-GYO)

         GO TO  GMN-SETB-EXIT
     END-IF.

*I↓
     MOVE  SPACE                      TO  DSP-SELNO  (IX-GYO).
*I↑
     MOVE  TB-SEQ    (CT-PAGE IX-GYO) TO  DSP-SEQ    (IX-GYO).
     MOVE  TB-RENNO  (CT-PAGE IX-GYO) TO  DSP-RENNO  (IX-GYO).
     MOVE  TB-BASHO  (CT-PAGE IX-GYO) TO  DSP-BASHO  (IX-GYO).
     MOVE  TB-TORCD  (CT-PAGE IX-GYO) TO  DSP-TORCDM (IX-GYO).
     MOVE  TB-SDENNO (CT-PAGE IX-GYO) TO  DSP-SDENNO (IX-GYO).
     MOVE  TB-KARA   (CT-PAGE IX-GYO) TO  DSP-KARA   (IX-GYO).
     MOVE  TB-EDENNO (CT-PAGE IX-GYO) TO  DSP-EDENNO (IX-GYO).
     MOVE  TB-NOHNBI (CT-PAGE IX-GYO) TO  DSP-NOHNBI (IX-GYO).
     MOVE  TB-JNOHBI (CT-PAGE IX-GYO) TO  DSP-JNOHBI (IX-GYO).

   *> プロテクトしない。
     MOVE  SPACE            TO  EDIT-STATUS OF DSP-SELNO (IX-GYO).

   *> 連携Ｎｏの色を設定する。
     EVALUATE  TRUE
       WHEN  TB-STS (CT-PAGE IX-GYO) = "1"  *> 未送信、赤
         MOVE  "R"          TO  EDIT-COLOR OF DSP-RENNO (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-RENNO (IX-GYO)
       WHEN  TB-STS (CT-PAGE IX-GYO) = "2"  *> 苗側未取込、黄
         MOVE  "Y"          TO  EDIT-COLOR OF DSP-RENNO (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-RENNO (IX-GYO)
       WHEN  TB-STS (CT-PAGE IX-GYO) = "4"  *> 未取消、青
*I*******MOVE  "B"          TO  EDIT-COLOR OF DSP-RENNO (IX-GYO)
         MOVE  "W"          TO  EDIT-COLOR OF DSP-RENNO (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-RENNO (IX-GYO)
       WHEN  TB-STS (CT-PAGE IX-GYO) = "5"  *> 取消済み、桃
         MOVE  "P"          TO  EDIT-COLOR OF DSP-RENNO (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-RENNO (IX-GYO)
       WHEN  OTHER                 *> 取込済、無色
         MOVE  "M"          TO  EDIT-COLOR OF DSP-RENNO (IX-GYO)
         MOVE  "M"          TO  EDIT-OPTION OF DSP-RENNO (IX-GYO)
     END-EVALUATE.

 GMN-SETB-EXIT.
     EXIT.
****************************************************************
*  明細項目（オンライン）入力  ( PSW = 4 )                     *
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
         MOVE  "3"          TO  PSW

*    取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC
*I****** MOVE  "3"          TO  PSW

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

 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE "BODY-CHK-SEC"    TO  S-NAME.

     MOVE  ZERO             TO  CT-SEL.
     MOVE  ZERO             TO  FG-SEL.
     PERFORM  VARYING IX-GYO  FROM 1 BY 1
              UNTIL   IX-GYO  > 12
       PERFORM  BODY-CHKB-SEC

     END-PERFORM.

     IF  CT-SEL = ZERO *> 選択なし
         IF  ERR-FLG = ZERO
             MOVE  7        TO  ERR-FLG
         END-IF
     END-IF.

* チェックＯＫ
     IF  ERR-FLG NOT = ZERO
         GO TO  BODY-CHK-EXIT
     END-IF.

     MOVE  13               TO  ERR-FLG.
     MOVE  "5"              TO  PSW.
 BODY-CHK-EXIT.
     EXIT.

****************************************************************
*             明細項目チェックＢ                               *
****************************************************************
 BODY-CHKB-SEC         SECTION.
     MOVE "BODY-CHKB-SEC"   TO  S-NAME.

     IF  DSP-RENNO (IX-GYO) = SPACE *> データ無し行
         GO TO  BODY-CHKB-EXIT
     END-IF.

     MOVE  "M"         TO  EDIT-OPTION OF DSP-SELNO (IX-GYO).
     MOVE  SPACE       TO  EDIT-CURSOR OF DSP-SELNO (IX-GYO).

     IF  DSP-SELNO (IX-GYO) = SPACE *> 選択無し行
         GO TO  BODY-CHKB-EXIT
     END-IF.

     IF  DSP-SELNO (IX-GYO) = "Y"
         CONTINUE
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  9        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SELNO (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SELNO (IX-GYO)
         GO TO  BODY-CHKB-EXIT
     END-IF.

     ADD  1   TO  CT-SEL.

     IF  FG-SEL = ZERO
         MOVE  1            TO  FG-SEL
         MOVE  IX-GYO       TO  WK-SELGYO
     ELSE
         IF  ERR-FLG = ZERO
             MOVE  8        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SELNO (IX-GYO)
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SELNO (IX-GYO)
     END-IF.

 BODY-CHKB-EXIT.
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
*  確認処理入力 （ PSW = 5 ）
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE  "DSP-KAKU-SEC"   TO  S-NAME.

     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.

     EVALUATE  DSP-FNC
*    実行
       WHEN  "E000"
         PERFORM  PARA-OT-SEC
*I*↓****MOVE  "END"        TO  END-FLG
         MOVE  DSP-FNA00701 TO  WK-FNA00701
         CLOSE DSPFILE
         CLOSE HTOKMS
         CLOSE NARKANF
         CLOSE HTANMS
         CALL "SNA0080A" USING  PAA-TANCD
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
                                PAA-RENKEINO
         OPEN  I-O   DSPFILE
         OPEN  INPUT HTOKMS
         OPEN  INPUT NARKANF
         OPEN  INPUT HTANMS
         MOVE  WK-FNA00701  TO  DSP-FNA00701
         MOVE  "4"          TO  PSW
*I*↑********************************************
*    終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*    項目戻し
       WHEN  "F006"
         MOVE  "4"          TO  PSW
*    取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC
*I****** MOVE  "1"          TO  PSW

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

* 部門・担当者ＣＤ
     MOVE  SPACE            TO  PAA-TANCD.
     STRING
       PAR-BMNCD-IN  DSP-TANCD
       DELIMITED BY SIZE  INTO  PAA-TANCD.

* オンライン/手書種別
     MOVE  "1"              TO  PAA-ONLTEG.

* バッチＮｏ
     MOVE  ZERO             TO  PAA-BATNO.

* 出荷場所
     MOVE  DSP-BASHO (WK-SELGYO)  TO  PAA-BASHO.

* 納品日
     MOVE  DSP-NOHNBI (WK-SELGYO) TO  PAA-NOHNBI.

* 実納品日
     MOVE  DSP-JNOHBI (WK-SELGYO) TO  PAA-JNOHNBI.

* 取引先ＣＤ
     MOVE  DSP-TORCDM (WK-SELGYO) TO  PAA-TORCD.

* 伝票Ｎｏ１（ＦＲＯＭ）
     MOVE  DSP-SDENNO (WK-SELGYO) TO  PAA-DEN1-FROM.
* 伝票Ｎｏ１（ＴＯ）
     MOVE  DSP-EDENNO (WK-SELGYO) TO  PAA-DEN1-TO.

* 伝票Ｎｏ２（ＦＲＯＭ）
     MOVE  TB-SDENNO2 (CT-PAGE WK-SELGYO) TO  PAA-DEN2-FROM.
* 伝票Ｎｏ２（ＴＯ）
     MOVE  TB-EDENNO2 (CT-PAGE WK-SELGYO) TO  PAA-DEN2-TO.

* 伝票Ｎｏ３（ＦＲＯＭ）
     MOVE  TB-SDENNO3 (CT-PAGE WK-SELGYO) TO  PAA-DEN3-FROM.
* 伝票Ｎｏ３（ＴＯ）
     MOVE  TB-SDENNO3 (CT-PAGE WK-SELGYO) TO  PAA-DEN3-TO.

* 伝票Ｎｏ４（ＦＲＯＭ）
     MOVE  TB-SDENNO4 (CT-PAGE WK-SELGYO) TO  PAA-DEN4-FROM.
* 伝票Ｎｏ４（ＴＯ）
     MOVE  TB-SDENNO4 (CT-PAGE WK-SELGYO) TO  PAA-DEN4-TO.

* 伝票Ｎｏ５（ＦＲＯＭ）
     MOVE  TB-SDENNO5 (CT-PAGE WK-SELGYO) TO  PAA-DEN5-FROM.
* 伝票Ｎｏ５（ＴＯ）
     MOVE  TB-SDENNO5 (CT-PAGE WK-SELGYO) TO  PAA-DEN5-TO.

* 状態
     MOVE  TB-STS (CT-PAGE WK-SELGYO)  TO  PAA-STS.

* 連携Ｎｏ
     MOVE  DSP-RENNO (WK-SELGYO)  TO  PAA-RENKEINO.

 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  HTOKMS.
     CLOSE  NARKANF.
     CLOSE  HTANMS.
     CLOSE  DSPFILE.
*
 END-EXIT.
     EXIT.
*****************<<  SNA0070I   END PROGRAM  >>******************
