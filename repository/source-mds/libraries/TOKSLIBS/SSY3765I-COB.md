# SSY3765I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3765I.COB`

## ソースコード

```cobol
***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：ＴＲＡＮＴＲＡＮ連携データ作成指示 *
*    作成日／更新日　：2010/10/05                         *
*    作成者／更新者　：ＮＡＶ飯田                         *
*    処理概要　　　　：                                   *
*      ＴＲＡＮＴＲＡＮ連携データ処理条件を入力し         *
*      チェックＯＫなら条件をパラメータに出力する。       *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3765I.
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
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.

*箱数ファイル
     SELECT  NFHAKOF   ASSIGN    TO        NFHAKOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       HK1-F01
                                           HK1-F05
                                           HK1-F06
                                           HK1-F07
                                           HK1-F08
                       FILE      STATUS    HK1-ST.
*箱数ファイル２
     SELECT  NFHAKOF2  ASSIGN    TO        NFHAKOL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       HK2-F02
                                           HK2-F03
                                           HK2-F04
                                           HK2-F05
                                           HK2-F06
                                           HK2-F07
                                           HK2-F08
                       FILE      STATUS    HK2-ST.
*数量訂正ファイル
     SELECT  NFSUTEF   ASSIGN    TO        NFSUTEL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ST1-F01
                                           ST1-F05
                                           ST1-F06
                                           ST1-F07
                                           ST1-F08
                                           ST1-F09
                       FILE      STATUS    ST1-ST.
*数量訂正ファイル２
     SELECT  NFSUTEF2  ASSIGN    TO        NFSUTEL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ST2-F02
                                           ST2-F03
                                           ST2-F04
                                           ST2-F05
                                           ST2-F06
                                           ST2-F07
                                           ST2-F08
                                           ST2-F09
                       FILE      STATUS    ST2-ST.
*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SKB-F01
                       FILE      STATUS    SKB-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37651  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 箱数ファイル                                       *
****************************************************************
 FD  NFHAKOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK1       AS   PREFIX.
****************************************************************
*    FILE = 箱数ファイル２                                     *
****************************************************************
 FD  NFHAKOF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFHAKOF   OF   XFDLIB
                       JOINING   HK2       AS   PREFIX.

****************************************************************
*    FILE = 数量訂正ファイル                                   *
****************************************************************
 FD  NFSUTEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSUTEF   OF   XFDLIB
                       JOINING   ST1       AS   PREFIX.
****************************************************************
*    FILE = 数量訂正ファイル２                                 *
****************************************************************
 FD  NFSUTEF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSUTEF   OF   XFDLIB
                       JOINING   ST2       AS   PREFIX.
****************************************************************
*    FILE = 作場マスタ                                         *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  HK1-ST                   PIC  X(02).
     03  HK2-ST                   PIC  X(02).
     03  ST1-ST                   PIC  X(02).
     03  ST2-ST                   PIC  X(02).
     03  SKB-ST                   PIC  X(02).
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
     03  FG-NFHAKOF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-NFHAKOF2-END          PIC  X(03)  VALUE  SPACE.
     03  FG-NFSUTEF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-NFSUTEF2-END          PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  FG-SAKUBAF-INV           PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
     03  WK-SEL-KANRNO            PIC  9(08)  VALUE  ZERO.
     03  WK-RD-KANRNO             PIC  9(08)  VALUE  ZERO.
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
         NC"　".
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
     03  HK1-ERR           PIC N(20) VALUE
                        NC"箱数ファイル１エラー".
     03  HK2-ERR           PIC N(20) VALUE
                        NC"箱数ファイル２エラー".
     03  ST1-ERR           PIC N(20) VALUE
                        NC"数量訂正ファイル１エラー".
     03  ST2-ERR           PIC N(20) VALUE
                        NC"数量訂正ファイル２エラー".
     03  SKB-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
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
 01  PAR-TANCD             PIC  X(02).
* 出力パラメータ
 01  PAR-KBN               PIC  9(01).
 01  PAR-YMD               PIC  9(08).
 01  PAR-TIME              PIC  9(04).
 01  PAR-TORICD            PIC  9(08).
 01  PAR-KANRNO            PIC  9(08).
 01  PAR-SKBACD            PIC  X(02).
 01  PAR-SYKYMD            PIC  9(08).
 01  PAR-TENYMD            PIC  9(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-TANCD
                                         PAR-KBN
                                         PAR-YMD
                                         PAR-TIME
                                         PAR-TORICD
                                         PAR-KANRNO
                                         PAR-SKBACD
                                         PAR-SYKYMD
                                         PAR-TENYMD.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HK1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF.
     DISPLAY     HK1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HK2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFHAKOF2.
     DISPLAY     HK2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     HK2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ST1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTEF.
     DISPLAY     ST1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ST1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ST2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTEF2.
     DISPLAY     ST2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ST2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
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
     OPEN  I-O   NFHAKOF.
     OPEN  INPUT NFHAKOF2.
     OPEN  INPUT NFSUTEF.
     OPEN  INPUT NFSUTEF2.
     OPEN  INPUT SAKUBAF.
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
*********MOVE  99999999     TO  DSP-TORICD
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
     MOVE    "FSY37651"          TO   DSP-FMT.
     WRITE    DSP-FSY37651.
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

     IF  DSP-SYKYMD = ZERO
         MOVE  SPACE        TO  DSP-SYKYMD(1:8)
     END-IF.

     IF  DSP-TENYMD = ZERO
         MOVE  SPACE        TO  DSP-TENYMD(1:8)
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

     MOVE    "FSY37651"           TO   DSP-FMT.
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
*             明細項目（オンライン）　入力( PSW = 2 )          *
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
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYKYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYKYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENYMD.

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

     END-IF.
* 出荷日
     IF      EDIT-STATUS OF DSP-SYKYMD = SPACE
         AND DSP-SYKYMD NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-SYKYMD      TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE 1     TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SYKYMD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SYKYMD
             GO TO    BODY-CHK-EXIT
         END-IF
     END-IF.
* 店着日
     IF      EDIT-STATUS OF DSP-TENYMD = SPACE
         AND DSP-TENYMD NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-TENYMD      TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE 1     TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-TENYMD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TENYMD
             GO TO    BODY-CHK-EXIT
         END-IF
     END-IF.

     IF  ERR-FLG = ZERO
         PERFORM  NFHAKO-CHK-SEC
         IF  ERR-FLG NOT = ZERO
             GO TO  BODY-CHK-EXIT
         END-IF

         IF     DSP-SYKYMD NOT NUMERIC
             OR DSP-SYKYMD = ZERO
             CONTINUE
         ELSE
             PERFORM  NFSUTE-CHK-SEC
             IF  ERR-FLG NOT = ZERO
                 GO TO  BODY-CHK-EXIT
             END-IF
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
*    箱数ファイル存在チェック                                  *
****************************************************************
 NFHAKO-CHK-SEC         SECTION.
     IF  DSP-KBN = "1"  *> オンライン
         IF  DSP-YMD IS NUMERIC AND DSP-YMD NOT = ZERO
               *> バッチNO指定
             MOVE  LOW-VALUE     TO  FG-NFHAKOF2-END
             MOVE  DSP-YMD       TO  HK2-F02  *> バッチ日付
             MOVE  DSP-TIME      TO  HK2-F03  *> バッチ時間
             MOVE  DSP-TORICD    TO  HK2-F04  *> バッチ取引先
             MOVE  DSP-SKBACD    TO  HK2-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  HK2-F06  *> 店舗ＣＤ
             MOVE  SPACE         TO  HK2-F07  *> 納品場所
             MOVE  ZERO          TO  HK2-F08  *> 店着日
             PERFORM  RD-NFHAKOF2-SEC
             IF  FG-NFHAKOF2-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  5     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TIME
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TORICD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFHAKO-CHK-EXIT
             END-IF

             MOVE  HK2-F01       TO  WK-SEL-KANRNO

             IF     DSP-KANRNO NOT NUMERIC
                 OR DSP-KANRNO = ZERO
                 MOVE  HK2-F01   TO  DSP-KANRNO
             END-IF

         ELSE  *> 管理番号指定
             MOVE  LOW-VALUE     TO  FG-NFHAKOF-END
             MOVE  DSP-KANRNO    TO  WK-RD-KANRNO
             MOVE  DSP-KANRNO    TO  HK1-F01  *> 管理番号
             MOVE  DSP-SKBACD    TO  HK1-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  HK1-F06  *> 店舗ＣＤ
             MOVE  SPACE         TO  HK1-F07  *> 納品場所
             MOVE  ZERO          TO  HK1-F08  *> 店着日
             PERFORM  RD-NFHAKOF-SEC
             IF  FG-NFHAKOF-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  5     TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFHAKO-CHK-EXIT
             ELSE
                 IF HK1-F02 NOT = 99999999 *> バッチ日付
                    CONTINUE
                 ELSE
                     MOVE  5     TO  ERR-FLG
                     MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                     GO TO  NFHAKO-CHK-EXIT
                 END-IF

                 MOVE  HK1-F01   TO  WK-SEL-KANRNO
                 MOVE  HK1-F02   TO  DSP-YMD
                 MOVE  HK1-F03   TO  DSP-TIME
                 MOVE  HK1-F04   TO  DSP-TORICD

             END-IF
         END-IF
     ELSE               *> 手書き
         MOVE  LOW-VALUE    TO FG-NFHAKOF-END
         MOVE  DSP-KANRNO   TO  WK-RD-KANRNO
         MOVE  DSP-KANRNO   TO  HK1-F01  *> 管理番号
         MOVE  DSP-SKBACD   TO  HK1-F05  *> 作場ＣＤ
         MOVE  ZERO         TO  HK1-F06  *> 店舗ＣＤ
         MOVE  SPACE        TO  HK1-F07  *> 納品場所
         MOVE  ZERO         TO  HK1-F08  *> 店着日
         PERFORM  RD-NFHAKOF-SEC
         IF  FG-NFHAKOF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  5    TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION OF DSP-KANRNO
             GO TO  NFHAKO-CHK-EXIT
         ELSE
             IF HK1-F02 = 99999999 *> バッチ日付
                CONTINUE
             ELSE
                 MOVE  5     TO  ERR-FLG
                 MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFHAKO-CHK-EXIT
             END-IF

             MOVE  HK1-F01   TO  WK-SEL-KANRNO

         END-IF
     END-IF.

 NFHAKO-CHK-EXIT.
     EXIT.
****************************************************************
*    箱数ファイル２読込み　                                    *
****************************************************************
 RD-NFHAKOF2-SEC         SECTION.

     IF  FG-NFHAKOF2-END = LOW-VALUE
         START  NFHAKOF2  KEY >= HK2-F02
                                 HK2-F03
                                 HK2-F04
                                 HK2-F05
                                 HK2-F06
                                 HK2-F07
                                 HK2-F08
           INVALID KEY
              MOVE  "END"   TO  FG-NFHAKOF2-END
              GO TO  RD-NFHAKOF2-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFHAKOF2-END

     END-IF.

 RD-NFHAKOF2-010.
     READ  NFHAKOF2
       AT  END
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-READ.

     IF  HK2-F02 > DSP-YMD
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF  HK2-F03 > DSP-TIME
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF  HK2-F04 > DSP-TORICD
         MOVE  "END"        TO  FG-NFHAKOF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF     DSP-KANRNO  NOT NUMERIC
         OR DSP-KANRNO = ZERO
         CONTINUE
     ELSE
         IF  HK2-F01 NOT = DSP-KANRNO
             GO TO  RD-NFHAKOF2-010
         END-IF
     END-IF.

     IF      DSP-SKBACD NOT NUMERIC
         OR  DSP-SKBACD = ZERO
         CONTINUE
     ELSE
         IF  HK2-F05 > DSP-SKBACD
             MOVE  "END"        TO  FG-NFHAKOF2-END
             GO TO  RD-NFHAKOF2-EXIT
         END-IF
     END-IF.

     IF     DSP-TENYMD NOT NUMERIC
         OR DSP-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  HK2-F08 NOT = DSP-TENYMD
             GO TO  RD-NFHAKOF2-010
         END-IF
     END-IF.

     IF  HK2-F98 = SPACE  *> ＴＲＡＮＴＲＡＮ連携確定区分
         CONTINUE
     ELSE
         GO TO  RD-NFHAKOF2-010
     END-IF.

 RD-NFHAKOF2-EXIT.
     EXIT.

****************************************************************
*    箱数ファイル読込み　                                      *
****************************************************************
 RD-NFHAKOF-SEC          SECTION.

     IF  FG-NFHAKOF-END = LOW-VALUE
         START  NFHAKOF  KEY >= HK1-F01
                                HK1-F05
                                HK1-F06
                                HK1-F07
                                HK1-F08
           INVALID KEY
              MOVE  "END"   TO  FG-NFHAKOF-END
              GO TO  RD-NFHAKOF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFHAKOF-END

     END-IF.

 RD-NFHAKOF-010.
     READ  NFHAKOF
       AT  END
         MOVE  "END"        TO  FG-NFHAKOF-END
         GO TO  RD-NFHAKOF-EXIT
     END-READ.

     IF  HK1-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  FG-NFHAKOF-END
         GO TO  RD-NFHAKOF-EXIT
     END-IF.

     IF      DSP-SKBACD NOT NUMERIC
         OR  DSP-SKBACD = ZERO
         CONTINUE
     ELSE
         IF  HK1-F05 > DSP-SKBACD
             MOVE  "END"        TO  FG-NFHAKOF-END
             GO TO  RD-NFHAKOF-EXIT
         END-IF
     END-IF.

     IF     DSP-TENYMD NOT NUMERIC
         OR DSP-TENYMD = ZERO
         CONTINUE
     ELSE
         IF  HK1-F08 NOT = DSP-TENYMD
             GO TO  RD-NFHAKOF-010
         END-IF
     END-IF.

     IF  HK1-F98 = SPACE  *> ＴＲＡＮＴＲＡＮ連携確定区分
         CONTINUE
     ELSE
         GO TO  RD-NFHAKOF-010
     END-IF.

 RD-NFHAKOF-EXIT.
     EXIT.

****************************************************************
*    数量訂正ファイル存在チェック
****************************************************************
 NFSUTE-CHK-SEC         SECTION.
     IF  DSP-KBN = "1"  *> オンライン
         IF  DSP-YMD IS NUMERIC AND DSP-YMD NOT = ZERO
               *> バッチNO指定
             MOVE  LOW-VALUE     TO FG-NFSUTEF2-END
             MOVE  DSP-YMD       TO  ST2-F02  *> バッチ日付
             MOVE  DSP-TIME      TO  ST2-F03  *> バッチ時間
             MOVE  DSP-TORICD    TO  ST2-F04  *> バッチ取引先
             MOVE  DSP-SKBACD    TO  ST2-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  ST2-F06  *> 店舗ＣＤ
             MOVE  SPACE         TO  ST2-F07  *> 納品場所
             MOVE  ZERO          TO  ST2-F08  *> 店着日
             MOVE  ZERO          TO  ST2-F09  *> 伝票番号
             PERFORM  RD-NFSUTEF2-SEC
             IF  FG-NFSUTEF2-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  5     TO  ERR-FLG
                 END-IF
                 MOVE  "C"       TO  EDIT-CURSOR OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-YMD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TIME
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-TORICD
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFSUTE-CHK-EXIT
             END-IF
         ELSE  *> 管理番号指定
             MOVE  LOW-VALUE     TO  FG-NFSUTEF-END
             MOVE  DSP-KANRNO    TO  ST1-F01  *> 管理番号
             MOVE  DSP-SKBACD    TO  ST1-F05  *> 作場ＣＤ
             MOVE  ZERO          TO  ST1-F06  *> 店舗ＣＤ
             MOVE  SPACE         TO  ST1-F07  *> 納品場所
             MOVE  ZERO          TO  ST1-F08  *> 店着日
             MOVE  ZERO          TO  ST1-F09  *> 伝票番号
             PERFORM  RD-NFSUTEF-SEC
             IF  FG-NFSUTEF-END = "END"
                 IF  ERR-FLG = ZERO
                     MOVE  5     TO  ERR-FLG
                 END-IF
                 MOVE  "R"       TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFSUTE-CHK-EXIT
             ELSE
                 IF ST1-F02 NOT = 99999999 *> バッチ日付
                    CONTINUE
                 ELSE
                     MOVE  5     TO  ERR-FLG
                     MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                     GO TO  NFSUTE-CHK-EXIT
                 END-IF
             END-IF
         END-IF
     ELSE               *> 手書き
         MOVE  LOW-VALUE    TO FG-NFSUTEF-END
         MOVE  DSP-KANRNO   TO  ST1-F01  *> 管理番号
         MOVE  DSP-SKBACD   TO  ST1-F05  *> 作場ＣＤ
         MOVE  ZERO         TO  ST1-F06  *> 店舗ＣＤ
         MOVE  SPACE        TO  ST1-F07  *> 納品場所
         MOVE  ZERO         TO  ST1-F08  *> 店着日
         MOVE  ZERO         TO  ST1-F09  *> 伝票番号
         PERFORM  RD-NFSUTEF-SEC
         IF  FG-NFSUTEF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  5    TO  ERR-FLG
             END-IF
             MOVE  "R"      TO  EDIT-OPTION OF DSP-KANRNO
             GO TO  NFSUTE-CHK-EXIT
         ELSE
             IF ST1-F02 = 99999999 *> バッチ日付
                CONTINUE
             ELSE
                 MOVE  5     TO  ERR-FLG
                 MOVE  "R"   TO  EDIT-OPTION OF DSP-KANRNO
                 GO TO  NFSUTE-CHK-EXIT
             END-IF
         END-IF
     END-IF.

 NFSUTE-CHK-EXIT.
     EXIT.
****************************************************************
*    箱数訂正ファイル２読込み　                                *
****************************************************************
 RD-NFSUTEF2-SEC         SECTION.

     IF  FG-NFSUTEF2-END = LOW-VALUE
         START  NFSUTEF2  KEY >= ST2-F02
                                 ST2-F03
                                 ST2-F04
                                 ST2-F05
                                 ST2-F06
                                 ST2-F07
                                 ST2-F08
                                 ST2-F09
           INVALID KEY
              MOVE  "END"   TO  FG-NFSUTEF2-END
             GO TO  RD-NFSUTEF2-EXIT
         END-START

        MOVE  SPACE         TO  FG-NFSUTEF2-END

     END-IF.

 RD-NFSUTEF2-010.
     READ  NFSUTEF2
       AT  END
         MOVE  "END"        TO  FG-NFSUTEF2-END
         GO TO  RD-NFSUTEF2-EXIT
     END-READ.

     IF  ST2-F02 > DSP-YMD
         MOVE  "END"        TO  FG-NFSUTEF2-END
         GO TO  RD-NFSUTEF2-EXIT
     END-IF.

     IF  ST2-F03 > DSP-TIME
         MOVE  "END"        TO  FG-NFSUTEF2-END
         GO TO  RD-NFHAKOF2-EXIT
     END-IF.

     IF  ST2-F04 > DSP-TORICD
         MOVE  "END"        TO  FG-NFSUTEF2-END
         GO TO  RD-NFSUTEF2-EXIT
     END-IF.

     IF     DSP-KANRNO  NOT NUMERIC
         OR DSP-KANRNO = ZERO
         CONTINUE
     ELSE
         IF  HK2-F01 NOT = DSP-KANRNO
             GO TO  RD-NFSUTEF2-010
         END-IF
     END-IF.

     IF  ST2-F05 > DSP-SKBACD
         MOVE  "END"        TO  FG-NFSUTEF2-END
         GO TO  RD-NFSUTEF2-EXIT
     END-IF.

     IF  ST2-F08 NOT = DSP-TENYMD
         GO TO  RD-NFSUTEF2-010
     END-IF.

     IF  ST2-F14 NOT = DSP-SYKYMD
         GO TO  RD-NFSUTEF2-010
     END-IF.

 RD-NFSUTEF2-EXIT.
     EXIT.

****************************************************************
*    数量訂正ファイル読込み　                                  *
****************************************************************
 RD-NFSUTEF-SEC          SECTION.

     IF  FG-NFSUTEF-END = LOW-VALUE
         START  NFSUTEF  KEY >= ST1-F01
                                 ST1-F05
                                 ST1-F06
                                 ST1-F07
                                 ST1-F08
                                 ST1-F09
           INVALID KEY
             MOVE  "END"    TO  FG-NFSUTEF-END
             GO TO  RD-NFSUTEF-EXIT
        END-START

        MOVE  SPACE         TO  FG-NFSUTEF-END

     END-IF.

 RD-NFSUTEF-010.
     READ  NFSUTEF
       AT  END
         MOVE  "END"        TO  FG-NFSUTEF-END
         GO TO  RD-NFSUTEF-EXIT
     END-READ.

     IF  ST1-F01 > DSP-KANRNO
         MOVE  "END"        TO  FG-NFSUTEF-END
         GO TO  RD-NFSUTEF-EXIT
     END-IF.

     IF  ST1-F05 > DSP-SKBACD
         MOVE  "END"        TO  FG-NFSUTEF-END
         GO TO  RD-NFSUTEF-EXIT
     END-IF.

     IF  ST1-F08 NOT = DSP-TENYMD
         GO TO  RD-NFSUTEF-010
     END-IF.

     IF  ST1-F14 NOT = DSP-SYKYMD
         GO TO  RD-NFSUTEF-010
     END-IF.

 RD-NFSUTEF-EXIT.
     EXIT.

****************************************************************
*             明細項目（手書き）　入力( PSW = 3 )              *
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
*             確認処理　入力（ PSW = 4 ）            2.4
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
         PERFORM  PARA-OT-SEC
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
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.
* 区分
     MOVE  DSP-KBN          TO  PAR-KBN.
* 日付
     MOVE  DSP-YMD          TO  PAR-YMD.
* 時間
     MOVE  DSP-TIME         TO  PAR-TIME.
* 取引先
     MOVE  DSP-TORICD       TO  PAR-TORICD.
* 管理番号
     IF DSP-KANRNO NOT NUMERIC
        MOVE  ZERO          TO  PAR-KANRNO
     ELSE
        MOVE  DSP-KANRNO    TO  PAR-KANRNO
     END-IF.
* 作場コード
     MOVE  DSP-SKBACD       TO  PAR-SKBACD
* 出荷日
     IF DSP-SYKYMD  NOT NUMERIC
        MOVE  ZERO          TO  PAR-SYKYMD
     ELSE
        MOVE  DSP-SYKYMD    TO  PAR-SYKYMD
     END-IF.
* 店着日
     IF DSP-TENYMD  NOT NUMERIC
        MOVE  ZERO          TO  PAR-TENYMD
     ELSE
        MOVE  DSP-TENYMD    TO  PAR-TENYMD
     END-IF.
* 箱数ファイル更新
     PERFORM  SDATE-GET-SEC.

     MOVE  LOW-VALUE        TO  FG-NFHAKOF-END.
     MOVE  WK-SEL-KANRNO    TO  WK-RD-KANRNO.
     MOVE  WK-SEL-KANRNO    TO  HK1-F01.  *> 管理番号
     MOVE  DSP-SKBACD       TO  HK1-F05.  *> 作場ＣＤ
     MOVE  ZERO             TO  HK1-F06.  *> 店舗ＣＤ
     MOVE  SPACE            TO  HK1-F07.  *> 納品場所
     MOVE  ZERO             TO  HK1-F08.  *> 店着日
     PERFORM  RD-NFHAKOF-SEC.
     PERFORM  UNTIL FG-NFHAKOF-END = "END"
           MOVE  "1"        TO  HK1-F98
           MOVE  SYS-DATE   TO  HK1-F99
           REWRITE HK1-REC
           PERFORM  RD-NFHAKOF-SEC
     END-PERFORM.

 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37651.
*ＰＧＩＤ
     MOVE  "SSY3765I"       TO  DSP-PGID.
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
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYKYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYKYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TENYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENYMD.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  NFHAKOF.
     CLOSE  NFHAKOF2.
     CLOSE  NFSUTEF.
     CLOSE  NFSUTEF2.
     CLOSE  SAKUBAF.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3765I   END PROGRAM  >>******************

```
