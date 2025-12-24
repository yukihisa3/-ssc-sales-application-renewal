# SSY3753I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3753I.COB`

## ソースコード

```cobol
***********************************************************
*                                                         *
*    顧客名          ：（株）サカタのタネ殿               *
*    サブシステム    ：ナフコＥＤＩ受信システム           *
*    業務名          ：ナフコＥＤＩ受信                   *
*    モジュール名    ：作場別出荷依頼データ出力（指示）   *
*    作成日／更新日  ：2010/10/08                         *
*    作成者／更新者  ：ＮＡＶ飯田                         *
*    処理概要        ：                                   *
*      入力した処理条件で基本情報ファイルを抽出し         *
*      出荷指示ファイルに編集／出力する。                 *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3753I.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2010/10/05.
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
       ASSIGN             TO  GS-DSPF
       FORMAT             IS  DSP-FMT
       GROUP              IS  DSP-GRP
       PROCESSING MODE    IS  DSP-PRO
       SELECTED FUNCTION  IS  DSP-FNC
       FILE STATUS        IS  DSP-ST.

*基本情報ファイル
     SELECT  NFJOHOF
       ASSIGN             TO  NFJOHOL1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  DYNAMIC
       RECORD KEY         IS  KH1-F01
                              KH1-F05
                              KH1-F06
                              KH1-F07
                              KH1-F08
                              KH1-F09
       FILE STATUS        IS  KH1-ST.
*基本情報ファイル２
     SELECT  NFJOHOF2
       ASSIGN             TO  NFJOHOL2
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  DYNAMIC
       RECORD KEY         IS  KH2-F02
                              KH2-F03
                              KH2-F04
                              KH2-F05
                              KH2-F06
                              KH2-F07
                              KH2-F08
                              KH2-F09
       FILE STATUS        IS  KH2-ST.
*出荷情報ファイル
     SELECT  NFSYUKF
       ASSIGN             TO  NFSYUKL1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  DYNAMIC
       RECORD KEY         IS  SYK-F01 *> 管理番号
                              SYK-F05 *> 作場ＣＤ
                              SYK-F06 *> 店舗ＣＤ
                              SYK-F07 *> 伝票番号
                              SYK-F08 *> 行番号
                              SYK-F09 *> 納入日
       FILE STATUS        IS  SYK-ST.
*作場マスタ
     SELECT  SAKUBAF
       ASSIGN             TO  SAKUBAL1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  RANDOM
       RECORD KEY         IS  SKB-F01
       FILE STATUS        IS  SKB-ST.

*店舗マスタ
     SELECT  HTENMS
       ASSIGN             TO  TENMS1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  RANDOM
       RECORD KEY         IS  TEN-F52
                              TEN-F011
       FILE STATUS        IS  TEN-ST.
*商品名称マスタ
     SELECT  NFMEIMS
       ASSIGN             TO  NFMEIMS1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  RANDOM
       RECORD KEY         IS  SYO-F01  *> ナフコ商品コード
       FILE STATUS        IS  SYO-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37531  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 基本情報ファイル                                   *
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH1       AS   PREFIX.
****************************************************************
*    FILE = 基本情報ファイル２                                 *
****************************************************************
 FD  NFJOHOF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH2       AS   PREFIX.

****************************************************************
*    FILE = 出荷情報ファイル                                   *
****************************************************************
 FD  NFSYUKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSYUKF   OF   XFDLIB
                       JOINING   SYK       AS   PREFIX.
****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
*    FILE = 店舗マスタ                                         *
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  NFMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFMEIMS   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  KH1-ST                   PIC  X(02).
     03  KH2-ST                   PIC  X(02).
     03  SYK-ST                   PIC  X(02).
     03  SKB-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SYO-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  FG-NFJOHOF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-NFJOHOF2-END          PIC  X(03)  VALUE  SPACE.
     03  FG-NFSYUKF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-NFSYUKF2-END          PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  FG-SAKUBAF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-NFSYUKF-INV           PIC  9(01).
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
     03  WK-KANRNO                PIC  9(08).
     03  WK-RD-KANRNO             PIC  9(08).

***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*受信日付チェック用
* 01  JUSIN-DATE.
*    03  JUSIN-DATEY              PIC  9(04)  VALUE  ZERO.
*    03  JUSIN-DATEM              PIC  9(02)  VALUE  ZERO.
*    03  JUSIN-DATED              PIC  9(02)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).

*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(25)  VALUE
         NC"正しい値を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)  VALUE
         NC"区分を入力してください。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)  VALUE
         NC"バッチＮＯを入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)  VALUE
         NC"管理番号を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)  VALUE
         NC"対象データが存在しません。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)  VALUE
         NC"無効キーです。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(25)  VALUE
         NC"作場ＣＤを入力して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  14  PIC   N(25).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  KH1-ERR           PIC N(20) VALUE
                        NC"基本情報ファイル１エラー".
     03  KH2-ERR           PIC N(20) VALUE
                        NC"基本情報ファイル２エラー".
     03  SYK-ERR           PIC N(20) VALUE
                        NC"出荷情報ファイル１エラー".
     03  SKB-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  SYO-ERR           PIC N(20) VALUE
                        NC"商品名称マスタエラー".
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
 01  PAR-BMNCD             PIC  X(04). *> 処理部門コード
 01  PAR-TANCD             PIC  X(02). *> 処理担当者
 01  PAR-SYOKBN            PIC  X(01). *> 処理区分
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BMNCD
                                         PAR-TANCD
                                         PAR-SYOKBN.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     KH1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF2.
     DISPLAY     KH2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSYUKF.
     DISPLAY     SYK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFMEIMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.

     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.

     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     PERFORM  SDATE-GET-SEC.
*ファイルのＯＰＥＮ
     OPEN  I-O   DSPFILE.
     OPEN  I-O   NFJOHOF.
     OPEN  INPUT NFJOHOF2.
     OPEN  I-O   NFSYUKF.
     OPEN  INPUT SAKUBAF.
     OPEN  INPUT HTENMS.
     OPEN  INPUT NFMEIMS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*処理区分入力
         WHEN      "1"  PERFORM   DSP-HEAD1-SEC
*明細項目入力（オンライン）
         WHEN      "2"  PERFORM   DSP-BODY-SEC
*明細項目入力（手書き）
         WHEN      "3"  PERFORM   DSP-BODY2-SEC
*確認入力
         WHEN      "4"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE  "4010"     TO  PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-HEAD1-SEC
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.

     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  SYS-DATE              TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)          TO  DSP-STIME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE  PSW
*処理区分
       WHEN  "1"
         IF  PAR-SYOKBN = "1" *> 再処理
             MOVE NC"（再依頼）"     TO  DSP-SHORNM
         ELSE      *> 新規
             MOVE SPACE              TO  DSP-SHORNM
         END-IF

         MOVE  PF-MSG-R(1)  TO  DSP-PFGAID
*明細項目（オンライン）
       WHEN  "2"
         MOVE  PF-MSG-R(2)  TO  DSP-PFGAID
         PERFORM  DSP-ZERO-SPACE-SEC
*明細項目（手書き）
       WHEN  "3"
         MOVE  PF-MSG-R(2)  TO  DSP-PFGAID
         MOVE  99999999     TO  DSP-YMD
         MOVE  9999         TO  DSP-TIME
******** MOVE  99999999     TO  DSP-TORICD
         PERFORM  DSP-ZERO-SPACE-SEC
*確認
       WHEN  "4"
         MOVE  PF-MSG-R(2)  TO  DSP-PFGAID
         PERFORM  DSP-ZERO-SPACE-SEC

       WHEN  OTHER
         MOVE    SPACE      TO  DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY37531"          TO   DSP-FMT.
     WRITE    DSP-FSY37531.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             ゼロを空白表示する                               *
****************************************************************
 DSP-ZERO-SPACE-SEC          SECTION.
     IF  DSP-YMD = ZERO
         MOVE  SPACE        TO  DSP-YMD (1:8)
     END-IF.

     IF  DSP-TIME = ZERO
         MOVE  SPACE        TO  DSP-TIME (1:4)
     END-IF.

     IF  DSP-TORICD = ZERO
         MOVE  SPACE        TO  DSP-TORICD (1:8)
     END-IF.

     IF  DSP-KANRNO = ZERO
         MOVE  SPACE        TO  DSP-KANRNO (1:8)
     END-IF.

 DSP-ZERO-SPACE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
*    MOVE    "SCREEN"             TO   DSP-GRP.
     EVALUATE   PSW
*処理区分
         WHEN   "1"
                MOVE    "SMODE"   TO   DSP-GRP
*明細項目（オンライン）
         WHEN   "2"
                MOVE    "MAIN"    TO   DSP-GRP
*明細項目（手書き）
         WHEN   "3"
                MOVE    "MAIN2"   TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37531"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF     DSP-KBN NOT NUMERIC
         OR DSP-KBN = ZERO
         MOVE  2                 TO  ERR-FLG
         MOVE  SPACE             TO  DSP-KBN (1:1)
         GO TO  HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２以外はエラー
     IF  DSP-KBN = 1 OR  2
         IF  DSP-KBN = 1   *> オインライン
             MOVE  "2"           TO  PSW
         ELSE              *> 手書き
             MOVE  "3"           TO  PSW
         END-IF

*        同一モードでループさせるため
         MOVE  DSP-KBN           TO  SAV-SHORI
     ELSE
         MOVE  1                 TO  ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目（オンライン）  入力( PSW = 2 )
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   BODY-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE  "4010"     TO  PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE   "BODY-CHK-SEC"       TO  S-NAME.

     MOVE  "M"              TO  EDIT-OPTION OF DSP-YMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TIME.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TIME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORICD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORICD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KANRNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANRNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBACD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBACD.

* 日付
     IF  DSP-KBN = "1"  *> オンライン
         IF     (DSP-YMD     NOT NUMERIC  OR DSP-YMD    = ZERO)
            AND (DSP-TIME    NOT NUMERIC  OR DSP-TIME   = ZERO)
            AND (DSP-TORICD  NOT NUMERIC  OR DSP-TORICD = ZERO)
            AND (DSP-KANRNO  NOT NUMERIC  OR DSP-KANRNO = ZERO)
            MOVE  3              TO  ERR-FLG
            MOVE  "C"            TO  EDIT-CURSOR OF DSP-YMD
            MOVE  "R"            TO  EDIT-OPTION OF DSP-YMD
            MOVE  "R"            TO  EDIT-OPTION OF DSP-TIME
            MOVE  "R"            TO  EDIT-OPTION OF DSP-TORICD
            MOVE  "R"            TO  EDIT-OPTION OF DSP-KANRNO
            GO TO  BODY-CHK-EXIT
         END-IF

         IF     DSP-YMD NOT NUMERIC
             OR DSP-YMD = ZERO
             CONTINUE
         ELSE
             MOVE  "2"           TO  LINK-IN-KBN
             MOVE  DSP-YMD       TO  LINK-IN-YMD8
             CALL  "SKYDTCKB" USING LINK-IN-KBN
                                    LINK-IN-YMD6
                                    LINK-IN-YMD8
                                    LINK-OUT-RET
                                    LINK-OUT-YMD
             IF  LINK-OUT-RET NOT = ZERO
                 IF  ERR-FLG = ZERO
                     MOVE  1     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-YMD
                 GO TO    BODY-CHK-EXIT
             END-IF
         END-IF
* 時間

* 取引先
         IF      (DSP-YMD  IS NUMERIC AND DSP-YMD NOT = ZERO)
             AND (DSP-TORICD  NOT NUMERIC OR DSP-TORICD = ZERO)
             IF  ERR-FLG = ZERO
                 MOVE  1         TO  ERR-FLG
             END-IF
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-TORICD
             MOVE  "R"           TO  EDIT-OPTION OF DSP-TORICD
             GO TO  BODY-CHK-EXIT
         END-IF

* 管理番号

     ELSE               *> 手書き
* 管理番号
         IF      DSP-KANRNO  NOT NUMERIC
             OR  DSP-KANRNO = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  4         TO  ERR-FLG
             END-IF
             MOVE  SPACE         TO  DSP-KANRNO(1:8)
             MOVE  "C"           TO  EDIT-CURSOR OF DSP-KANRNO
             MOVE  "R"           TO  EDIT-OPTION OF DSP-KANRNO
             GO TO  BODY-CHK-EXIT
         END-IF
     END-IF.

* 作場ＣＤ
     IF  DSP-SKBACD NOT = SPACE
         MOVE  DSP-SKBACD   TO  SKB-F01
         PERFORM  RD-SAKUBAF-SEC
         IF  FG-SAKUBAF-INV = 1
             MOVE  1        TO  ERR-FLG
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SKBACD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SKBACD
             GO TO  BODY-CHK-EXIT
         END-IF
     ELSE
         MOVE  8        TO  ERR-FLG
         MOVE  "C"      TO  EDIT-CURSOR OF DSP-SKBACD
         MOVE  "R"      TO  EDIT-OPTION OF DSP-SKBACD
         GO TO  BODY-CHK-EXIT
     END-IF.

     IF  ERR-FLG = ZERO
         PERFORM  NFJOHO-CHK-SEC
         IF  ERR-FLG NOT = ZERO
             GO TO  BODY-CHK-EXIT
         END-IF
     END-IF.

     MOVE  "4"                   TO  PSW.

 BODY-CHK-EXIT.
     EXIT.

****************************************************************
*    作場マスタ検索                                            *
****************************************************************
 RD-SAKUBAF-SEC          SECTION.
     READ  SAKUBAF
       INVALID
         MOVE  1                 TO  FG-SAKUBAF-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-SAKUBAF-INV
     END-READ.

 RD-SAKUBAF-EXIT.
     EXIT.

****************************************************************
*    基本情報ファイル存在チェック                              *
****************************************************************
 NFJOHO-CHK-SEC         SECTION.
     IF  DSP-KBN = "1"  *> オンライン
         IF  DSP-YMD IS NUMERIC AND DSP-YMD NOT = ZERO
               *> バッチNO指定
             MOVE  LOW-VALUE     TO FG-NFJOHOF2-END
             MOVE  DSP-YMD       TO  KH2-F02  *> バッチ日付
             MOVE  DSP-TIME      TO  KH2-F03  *> バッチ時間
             MOVE  DSP-TORICD    TO  KH2-F04  *> バッチ取引先
             MOVE  DSP-SKBACD    TO  KH2-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  KH2-F06  *> 店舗ＣＤ
             MOVE  ZERO          TO  KH2-F07  *> 納品場所
             MOVE  ZERO          TO  KH2-F08  *> 店着日
             PERFORM  RD-NFJOHOF2-SEC
             IF  FG-NFJOHOF2-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  5     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TIME
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TORICD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFJOHO-CHK-EXIT
             END-IF

             MOVE  KH2-F01       TO  WK-KANRNO
             IF     DSP-KANRNO NOT NUMERIC
                 OR DSP-KANRNO = ZERO
                 MOVE  KH2-F01   TO  DSP-KANRNO
             END-IF

         ELSE  *> 管理番号指定
             MOVE  LOW-VALUE     TO  FG-NFJOHOF-END
             MOVE  DSP-KANRNO    TO  WK-RD-KANRNO
             MOVE  DSP-KANRNO    TO  KH1-F01  *> 管理番号
             MOVE  DSP-SKBACD    TO  KH1-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  KH1-F06  *> 店舗ＣＤ
             MOVE  ZERO          TO  KH1-F07  *> 納品場所
             MOVE  ZERO          TO  KH1-F08  *> 店着日
             PERFORM  RD-NFJOHOF-SEC
             IF  FG-NFJOHOF-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  5     TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFJOHO-CHK-EXIT
             ELSE
                 IF KH1-F02 NOT = 99999999 *> バッチ日付
                    CONTINUE
                 ELSE
                     MOVE  5     TO  ERR-FLG
                     MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                     GO TO  NFJOHO-CHK-EXIT
                 END-IF
             END-IF

             MOVE  KH1-F01       TO  WK-KANRNO
             MOVE  KH1-F02       TO  DSP-YMD
             MOVE  KH1-F03       TO  DSP-TIME
             MOVE  KH1-F04       TO  DSP-TORICD

         END-IF
     ELSE               *> 手書き
         MOVE  LOW-VALUE    TO  FG-NFJOHOF-END
         MOVE  DSP-KANRNO   TO  WK-RD-KANRNO
         MOVE  DSP-KANRNO   TO  KH1-F01  *> 管理番号
         MOVE  DSP-SKBACD   TO  KH1-F05  *> 作場ＣＤ
         MOVE  ZERO         TO  KH1-F06  *> 店舗ＣＤ
         MOVE  ZERO         TO  KH1-F07  *> 納品場所
         MOVE  ZERO         TO  KH1-F08  *> 店着日
         PERFORM  RD-NFJOHOF-SEC
         IF  FG-NFJOHOF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  5    TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION OF DSP-KANRNO
             GO TO  NFJOHO-CHK-EXIT
         ELSE
             IF KH1-F02 = 99999999 *> バッチ日付
                CONTINUE
             ELSE
                 MOVE  5     TO  ERR-FLG
                 MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFJOHO-CHK-EXIT
             END-IF
         END-IF

         MOVE  KH1-F01       TO  WK-KANRNO

     END-IF.


 NFJOHO-CHK-EXIT.
     EXIT.
****************************************************************
*    基本情報ファイル２読込み                                  *
****************************************************************
 RD-NFJOHOF2-SEC         SECTION.

     IF  FG-NFJOHOF2-END = LOW-VALUE
         START  NFJOHOF2  KEY >= KH2-F02
                                 KH2-F03
                                 KH2-F04
                                 KH2-F05
                                 KH2-F06
                                 KH2-F07
                                 KH2-F08
                                 KH2-F09
           INVALID KEY
              MOVE  "END"   TO  FG-NFJOHOF2-END
              GO TO  RD-NFJOHOF2-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFJOHOF2-END

     END-IF.

 RD-NFJOHOF2-010.
     READ  NFJOHOF2  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-READ.

     IF  KH2-F02 > DSP-YMD
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  KH2-F03 > DSP-TIME
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  KH2-F04 > DSP-TORICD
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF     DSP-KANRNO  NOT NUMERIC
         OR DSP-KANRNO = ZERO
         CONTINUE
     ELSE
         IF  KH2-F01 NOT = DSP-KANRNO
             GO TO  RD-NFJOHOF2-010
         END-IF
     END-IF.

     IF  KH2-F05 NOT = DSP-SKBACD
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  PAR-SYOKBN = SPACE  *> 新規処理
*********IF  KH2-F24 = SPACE  *> 出荷指示作成区分
         IF  KH2-F26 = SPACE  *> 出荷確定ＤＴ区分
             CONTINUE
         ELSE
             GO TO  RD-NFJOHOF2-010
         END-IF
     END-IF.


 RD-NFJOHOF2-EXIT.
     EXIT.

****************************************************************
*    基本情報ファイル読込み                                    *
****************************************************************
 RD-NFJOHOF-SEC          SECTION.


     IF  FG-NFJOHOF-END = LOW-VALUE
         START  NFJOHOF  KEY >= KH1-F01
                                KH1-F05
                                KH1-F06
                                KH1-F07
                                KH1-F08
                                KH1-F09
           INVALID KEY
              MOVE  "END"   TO  FG-NFJOHOF-END
              GO TO  RD-NFJOHOF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFJOHOF-END

     END-IF.

 RD-NFJOHOF-010.
     READ  NFJOHOF  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFJOHOF-END
         GO TO  RD-NFJOHOF-EXIT
     END-READ.

     IF  KH1-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFJOHOF-END
         GO TO  RD-NFJOHOF-EXIT
     END-IF.

     IF  KH1-F05 NOT = DSP-SKBACD
         MOVE  "END"        TO  FG-NFJOHOF2-END
         GO TO  RD-NFJOHOF2-EXIT
     END-IF.

     IF  PAR-SYOKBN = SPACE  *> 新規処理
*********IF  KH1-F24 = SPACE  *> 出荷指示作成区分
         IF  KH1-F26 = SPACE  *> 出荷確定ＤＴ区分
             CONTINUE
         ELSE
             GO TO  RD-NFJOHOF-010
         END-IF
     END-IF.

     IF  DSP-KBN = "1"  *> オンライン
         IF KH1-F02     = 99999999 *> バッチ日付
            GO TO  RD-NFJOHOF-010
         END-IF
     ELSE               *> 手書き
         IF KH1-F02 NOT = 99999999 *> バッチ日付
            GO TO  RD-NFJOHOF-010
         END-IF
     END-IF.

 RD-NFJOHOF-EXIT.
     EXIT.

****************************************************************
*             明細項目（手書き）  入力( PSW = 3 )
****************************************************************
 DSP-BODY2-SEC          SECTION.
     MOVE     "DSP-BODY2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  BODY-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE  "4010"     TO  PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY2-EXIT.
     EXIT.


****************************************************************
*             確認処理  入力（ PSW = 4 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE  DSP-FNC
*実行
       WHEN  "E000"
         PERFORM  UPD-SEC
         MOVE  "END"        TO  END-FLG
*終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*項目戻し
       WHEN  "F006"
         IF  DSP-KBN = "1"  *> オンライン
             MOVE  "2"      TO  PSW
         ELSE               *> 手書き
             MOVE  "3"      TO  PSW
         END-IF
*取消
       WHEN  "F004"
         MOVE  "1"          TO  PSW
         PERFORM  INIT-DSP-SEC

       WHEN  OTHER
         MOVE  6            TO  ERR-FLG
         GO TO  DSP-KAKU-SEC

     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    出力処理                                                  *
****************************************************************
 UPD-SEC         SECTION.
* 出荷情報ファイル対象レコード初期削除
     PERFORM  SDATE-GET-SEC.

     MOVE  LOW-VALUE        TO  FG-NFSYUKF-END.
     MOVE  WK-KANRNO        TO  SYK-F01.  *> 管理番号
     MOVE  DSP-SKBACD       TO  SYK-F05.  *> 作場ＣＤ
     MOVE  ZERO             TO  SYK-F06.  *> 店舗ＣＤ
     MOVE  ZERO             TO  SYK-F07.  *> 納品場所
     MOVE  ZERO             TO  SYK-F08.  *> 店着日
     PERFORM  RD-NFSYUKF-SEC.
     PERFORM  UNTIL FG-NFSYUKF-END = "END"
       DELETE  NFSYUKF

       PERFORM  RD-NFSYUKF-SEC
     END-PERFORM.

* 出荷情報ファイル出力

     MOVE  LOW-VALUE        TO  FG-NFJOHOF-END.
     MOVE  WK-KANRNO        TO  WK-RD-KANRNO.
     MOVE  WK-KANRNO        TO  KH1-F01.  *> 管理番号
     MOVE  DSP-SKBACD       TO  KH1-F05.  *> 作場ＣＤ
     MOVE  ZERO             TO  KH1-F06.  *> 店舗ＣＤ
     MOVE  ZERO             TO  KH1-F07.  *> 納品場所
     MOVE  ZERO             TO  KH1-F08.  *> 店着日
     PERFORM  RD-NFJOHOF-SEC.
     PERFORM  UNTIL FG-NFJOHOF-END = "END"
       PERFORM  NFSYUKF-EDWT-SEC

       PERFORM  RD-NFJOHOF-SEC
     END-PERFORM.

 UPD-EXIT.
     EXIT.
****************************************************************
*    出荷情報ファイル読込み                                    *
****************************************************************
 RD-NFSYUKF-SEC          SECTION.

     IF  FG-NFSYUKF-END = LOW-VALUE
         START  NFSYUKF  KEY >= SYK-F01
                                SYK-F05
                                SYK-F06
                                SYK-F07
                                SYK-F08
                                SYK-F09
           INVALID KEY
              MOVE  "END"   TO  FG-NFSYUKF-END
              GO TO  RD-NFSYUKF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFSYUKF-END

     END-IF.

 RD-NFSYUKF-010.
     READ  NFSYUKF  NEXT
       AT  END
         MOVE  "END"        TO  FG-NFSYUKF-END
         GO TO  RD-NFSYUKF-EXIT
     END-READ.

     IF  SYK-F01 > WK-KANRNO
         MOVE  "END"        TO  FG-NFSYUKF-END
         GO TO  RD-NFSYUKF-EXIT
     END-IF.

     IF      DSP-SKBACD NOT NUMERIC
         OR  DSP-SKBACD = ZERO
         CONTINUE
     ELSE
         IF  SYK-F05 > DSP-SKBACD
             MOVE  "END"        TO  FG-NFSYUKF-END
             GO TO  RD-NFSYUKF-EXIT
         END-IF
     END-IF.

 RD-NFSYUKF-EXIT.
     EXIT.

****************************************************************
*  出荷情報ファイル更新                                        *
****************************************************************
 NFSYUKF-EDWT-SEC          SECTION.

     MOVE  KH1-F01     TO  SYK-F01. *> 管理番号
     MOVE  KH1-F05     TO  SYK-F05. *> 倉庫ＣＤ（作場ＣＤ
     MOVE  KH1-F06     TO  SYK-F06. *> 店舗ＣＤ
     MOVE  KH1-F07     TO  SYK-F07. *> 伝票番号
     MOVE  KH1-F08     TO  SYK-F08. *> 行番号
     MOVE  KH1-F09     TO  SYK-F09. *> 納品日

     READ  NFSYUKF  KEY IS SYK-F01
                           SYK-F05
                           SYK-F06
                           SYK-F07
                           SYK-F08
                           SYK-F09
       INVALID
          MOVE  1           TO  FG-NFSYUKF-INV
       NOT INVALID
          MOVE  ZERO        TO  FG-NFSYUKF-INV
     END-READ.



     IF  FG-NFSYUKF-INV = 1
         MOVE  KH1-F01      TO  SYK-F01
         MOVE  KH1-F05      TO  SYK-F05
         MOVE  KH1-F06      TO  SYK-F06
         MOVE  KH1-F07      TO  SYK-F07
         MOVE  KH1-F08      TO  SYK-F08
         MOVE  KH1-F09      TO  SYK-F09
         PERFORM  NFSYUKF-ED-SEC
         WRITE SYK-REC
     ELSE
         PERFORM  NFSYUKF-ED-SEC
         REWRITE SYK-REC
     END-IF.

* 基本情報ファイル済み更新
     MOVE  "1"              TO  KH1-F24. *> 出荷指示作成区分
     MOVE  SYS-DATE         TO  KH1-F25. *> 出荷指示作成日
     MOVE  PAR-BMNCD        TO  KH1-F32. *> 更新担当者部門
     MOVE  PAR-TANCD        TO  KH1-F33. *> 更新担当者
     REWRITE KH1-REC.

 NFSYUKF-EDWT-EXIT.
     EXIT.
****************************************************************
*  出荷情報ファイル編集                                        *
****************************************************************
 NFSYUKF-ED-SEC          SECTION.
     MOVE  KH1-F01     TO  SYK-F01. *> 管理番号
     MOVE  KH1-F02     TO  SYK-F02. *> バッチ日付
     MOVE  KH1-F03     TO  SYK-F03. *> バッチ時間
     MOVE  KH1-F04     TO  SYK-F04. *> バッチ取引先
     MOVE  KH1-F05     TO  SYK-F05. *> 倉庫ＣＤ（作場ＣＤ
     MOVE  KH1-F06     TO  SYK-F06. *> 店舗ＣＤ
     MOVE  KH1-F07     TO  SYK-F07. *> 伝票番号
     MOVE  KH1-F08     TO  SYK-F08. *> 行番号
     MOVE  KH1-F09     TO  SYK-F09. *> 納品日
     MOVE  KH1-F10     TO  SYK-F10. *> 出荷日
     MOVE  KH1-F11     TO  SYK-F11. *> 入荷予定日
     MOVE  KH1-F12     TO  SYK-F12. *> 直送先ＣＤ
     MOVE  KH1-F13     TO  SYK-F13. *> 相手商品ＣＤ
     MOVE  KH1-F14     TO  SYK-F14. *> 相手ＪＡＮＣＤ
     MOVE  KH1-F15     TO  SYK-F15. *> サカタ商品ＣＤ
     MOVE  KH1-F16     TO  SYK-F16. *> サカタ単品ＣＤ
     MOVE  KH1-F17     TO  SYK-F17. *> オンライン／手書区
     MOVE  KH1-F18     TO  SYK-F18. *> 発注単位
     MOVE  KH1-F19     TO  SYK-F19. *> 発注数
     MOVE  KH1-F20     TO  SYK-F20. *> 訂正数
     MOVE  KH1-F21     TO  SYK-F21. *> 想定ケース数
     MOVE  KH1-F22     TO  SYK-F22. *> 訂正ケース数
     MOVE  KH1-F23     TO  SYK-F23. *> 訂正区分
     MOVE  "1"         TO  SYK-F24. *> 出荷指示作成区分
     MOVE  SYS-DATE    TO  SYK-F25. *> 出荷指示作成日
     MOVE  KH1-F26     TO  SYK-F26. *> 出荷確定ＤＴ区分
     MOVE  KH1-F27     TO  SYK-F27. *> 出荷確定ＤＴ日付
     MOVE  KH1-F28     TO  SYK-F28. *> 出荷場所
     MOVE  KH1-F29     TO  SYK-F29. *> _番
     MOVE  PAR-BMNCD   TO  SYK-F30. *> 登録担当者部門
     MOVE  PAR-TANCD   TO  SYK-F31. *> 登録担当者
     MOVE  SPACE       TO  SYK-F32. *> 更新担当者部門
     MOVE  SPACE       TO  SYK-F33. *> 更新担当者

* 作場名称
     MOVE  KH1-F05     TO  SKB-F01.
     PERFORM  RD-SAKUBAF-SEC.
     IF  FG-SAKUBAF-INV = 1
         MOVE  SPACE   TO  SYK-F34  *> 作場名称
     ELSE
         MOVE  SKB-F02 TO  SYK-F34  *> 作場名称
     END-IF.

* 商品名称
     MOVE  KH1-F13     TO  SYO-F01.
     READ  NFMEIMS
       INVALID
           MOVE  SPACE          TO  SYK-F35 *> 商品名称（１）
           MOVE  SPACE          TO  SYK-F36 *> 商品名称（２）
       NOT INVALID
           MOVE  SYO-F05(01:10) TO  SYK-F35 *> 商品名称（１）
           MOVE  SYO-F05(11:10) TO  SYK-F36 *> 商品名称（２）
     END-READ.

* 店舗名称
     MOVE  KH1-F04     TO  TEN-F52.  *> 相手先取引先コード
     MOVE  KH1-F06     TO  TEN-F011. *> 店舗コード

     READ  HTENMS
       INVALID
         MOVE  SPACE   TO  SYK-F37  *> 店舗名称
       NOT INVALID
         MOVE  TEN-F02 TO  SYK-F37  *> 店舗名称
     END-READ.

     MOVE  KH1-FIL     TO  SYK-FIL. *> 予備

                                  *> ヘッダーレコード
     MOVE  KH1-HE01    TO  SYK-HE01. *> レコード区分
     MOVE  KH1-HE02    TO  SYK-HE02. *> データ種別
     MOVE  KH1-HE03    TO  SYK-HE03. *> 予備
     MOVE  KH1-HE04    TO  SYK-HE04. *> 伝票番号
     MOVE  KH1-HE05    TO  SYK-HE05. *> 社コード
     MOVE  KH1-HE06    TO  SYK-HE06. *> 取引先コード
     MOVE  KH1-HE07    TO  SYK-HE07. *> 予備
     MOVE  KH1-HE08    TO  SYK-HE08. *> 予備
     MOVE  KH1-HE09    TO  SYK-HE09. *> 分類コード
     MOVE  KH1-HE10    TO  SYK-HE10. *> 伝票区分
     MOVE  KH1-HE11    TO  SYK-HE11. *> 発注日
     MOVE  KH1-HE12    TO  SYK-HE12. *> 納品日
     MOVE  KH1-HE13    TO  SYK-HE13. *> 取引先ＣＤ
     MOVE  KH1-HE14    TO  SYK-HE14. *> ステーションアドレ
     MOVE  KH1-HE15    TO  SYK-HE15. *> 社名
     MOVE  KH1-HE16    TO  SYK-HE16. *> 店名
     MOVE  KH1-HE17    TO  SYK-HE17. *> 取引先名
     MOVE  KH1-HE18    TO  SYK-HE18. *> 取引先電話番号
     MOVE  KH1-HE19    TO  SYK-HE19. *> 発注区分
     MOVE  KH1-HE20    TO  SYK-HE20. *> 納品場所
     MOVE  KH1-HE21    TO  SYK-HE21. *> 予備
     MOVE  KH1-HE22    TO  SYK-HE22. *> 伝票ヘッダオプショ

                                   *> ヘッダレコードオプ
     MOVE  KH1-HF01    TO  SYK-HF01. *> レコード区分
     MOVE  KH1-HF02    TO  SYK-HF02. *> データ種別
     MOVE  KH1-HF03    TO  SYK-HF03. *> Ａ欄
     MOVE  KH1-HF04    TO  SYK-HF04. *> Ｄ欄の上段
     MOVE  KH1-HF05    TO  SYK-HF05. *> Ｄ欄の下段左
     MOVE  KH1-HF06    TO  SYK-HF06. *> Ｄ欄の下段右
     MOVE  KH1-HF07    TO  SYK-HF07. *> Ｅ欄
     MOVE  KH1-HF08    TO  SYK-HF08. *> Ｆ欄の下段
     MOVE  KH1-HF09    TO  SYK-HF09. *> Ｆ欄の上段
     MOVE  KH1-HF10    TO  SYK-HF10. *> Ｌ欄の中段
     MOVE  KH1-HF11    TO  SYK-HF11. *> Ｌ欄の上段
     MOVE  KH1-HF12    TO  SYK-HF12. *> ルート
     MOVE  KH1-HF13    TO  SYK-HF13. *> Ｌ欄の下段
     MOVE  KH1-HF14    TO  SYK-HF14. *> 伝票ヘッダオプショ

                                   *> 明細レコード
     MOVE  KH1-ME01    TO  SYK-ME01. *> レコード区分
     MOVE  KH1-ME02    TO  SYK-ME02. *> データ種別
     MOVE  KH1-ME03    TO  SYK-ME03. *> 行番号
     MOVE  KH1-ME04    TO  SYK-ME04. *> ＪＡＮ商品コード
     MOVE  KH1-ME05    TO  SYK-ME05. *> 柄
     MOVE  KH1-ME06    TO  SYK-ME06. *> 予備
     MOVE  KH1-ME07    TO  SYK-ME07. *> 単位
     MOVE  KH1-ME08    TO  SYK-ME08. *> 数量
     MOVE  KH1-ME09    TO  SYK-ME09. *> 予備
     MOVE  KH1-ME10    TO  SYK-ME10. *> 原単価
     MOVE  KH1-ME11    TO  SYK-ME11. *> 予備
     MOVE  KH1-ME12    TO  SYK-ME12. *> 売単価
     MOVE  KH1-ME13    TO  SYK-ME13. *> 原価金額
     MOVE  KH1-ME14    TO  SYK-ME14. *> 売価金額
     MOVE  KH1-ME15    TO  SYK-ME15. *> 予備
     MOVE  KH1-ME16    TO  SYK-ME16. *> 商品名
     MOVE  KH1-ME17    TO  SYK-ME17. *> 発注商品コード
     MOVE  KH1-ME18    TO  SYK-ME18. *> 色
     MOVE  KH1-ME19    TO  SYK-ME19. *> サイズ
     MOVE  KH1-ME20    TO  SYK-ME20. *> 予備
     MOVE  KH1-ME21    TO  SYK-ME21. *> 明細ラインコード

                                   *> 明細レコードオプシ
     MOVE  KH1-MF01    TO  SYK-MF01. *> レコード区分
     MOVE  KH1-MF02    TO  SYK-MF02. *> データ種別
     MOVE  KH1-MF03    TO  SYK-MF03. *> 規格
     MOVE  KH1-MF04    TO  SYK-MF04. *> プライスシール発行
     MOVE  KH1-MF05    TO  SYK-MF05. *> 仕入先印刷区分
     MOVE  KH1-MF06    TO  SYK-MF06. *> 部門コード
     MOVE  KH1-MF07    TO  SYK-MF07. *> ＪＡＮ商品コード
     MOVE  KH1-MF08    TO  SYK-MF08. *> 商品名略
     MOVE  KH1-MF09    TO  SYK-MF09. *> 規格名略
     MOVE  KH1-MF10    TO  SYK-MF10. *> 売単価
     MOVE  KH1-MF11    TO  SYK-MF11. *> 出力枚数
     MOVE  KH1-MF12    TO  SYK-MF12. *> 予備
     MOVE  KH1-MF13    TO  SYK-MF13. *> 伝票明細オプション
 NFSYUKF-ED-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37531.
*ＰＧＩＤ
     MOVE  "SSY3753I"       TO  DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-YMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TIME.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TIME.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TORICD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TORICD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KANRNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANRNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SKBACD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SKBACD.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  NFJOHOF.
     CLOSE  NFJOHOF2.
     CLOSE  NFSYUKF.
     CLOSE  SAKUBAF.
     CLOSE  HTENMS.
     CLOSE  NFMEIMS.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3753I   END PROGRAM  >>******************

```
