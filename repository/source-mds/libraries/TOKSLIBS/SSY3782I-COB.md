# SSY3782I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3782I.COB`

## ソースコード

```cobol
***********************************************************
*    顧客名          ：（株）サカタのタネ殿               *
*    サブシステム    ：ナフコＥＤＩ受信システム           *
*    業務名          ：ナフコＥＤＩ受信                   *
*    モジュール名    ：受領書発行指示                     *
*    作成日／更新日  ：2011/01/27                         *
*    作成者／更新者  ：ＮＡＶ高橋                         *
*    処理概要        ：                                   *
*      受領書を発行する範囲を指定する。　　　　　         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3782I.
 AUTHOR.               T.T.
 DATE-WRITTEN.         2011/01/27.
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
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37821  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
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
         NC"開始が終了を超えています。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)  VALUE
         NC"無効キーです。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)  VALUE
         NC"日付エラーです。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
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
 01  PARA-KBN              PIC  X(01). *> 発行区分
 01  PARA-JYUSIN-ST        PIC  9(08). *> 受信日開始
 01  PARA-JYUSIN-ED        PIC  9(08). *> 受信日終了
 01  PARA-SIIRED-ST        PIC  9(08). *> 仕入計上日開始
 01  PARA-SIIRED-ED        PIC  9(08). *> 仕入計上日終了
 01  PARA-LIST-KBN         PIC  X(01). *> 帳票区分
*
**************************************************************
 PROCEDURE             DIVISION   USING  PARA-KBN
                                         PARA-JYUSIN-ST
                                         PARA-JYUSIN-ED
                                         PARA-SIIRED-ST
                                         PARA-SIIRED-ED
                                         PARA-LIST-KBN.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
*
     MOVE     "PROCESS START"     TO   S-NAME.
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
*
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
*明細項目入力（受信日）
         WHEN      "2"  PERFORM   DSP-BODY-SEC
*明細項目入力（仕入計上日）
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
*明細項目（受信日）
       WHEN  "2"  WHEN  "3"
         MOVE  PF-MSG-R(2)  TO  DSP-PFGAID
*確認
       WHEN  "4"
         MOVE  PF-MSG-R(2)  TO  DSP-PFGAID
       WHEN  OTHER
         MOVE    SPACE      TO  DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY37821"          TO   DSP-FMT.
     WRITE    DSP-FSY37821.
*
 DSP-WRITE-EXIT.
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
*明細項目（受信日）
         WHEN   "2"
                MOVE    "MAIN"    TO   DSP-GRP
*明細項目（仕入計上日）
         WHEN   "3"
                MOVE    "MAIN2"   TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37821"           TO   DSP-FMT.
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
         IF  DSP-KBN = 1   *> 受信日
             MOVE  "2"           TO  PSW
         ELSE              *> 仕入計上日
             MOVE  "3"           TO  PSW
         END-IF
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
                IF  ERR-FLG = ZERO
                    MOVE  "4"    TO   PSW
                ELSE
                    MOVE  "2"    TO   PSW
                END-IF
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
*
     MOVE   "BODY-CHK-SEC"       TO  S-NAME.
*
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JYUST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JYUED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SYUKBN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYUKBN.
*開始日付
     IF  DSP-JYUST NOT NUMERIC
     OR  DSP-JYUST = ZERO
         MOVE   ZERO        TO  DSP-JYUST
     ELSE
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-JYUST    TO  LINK-IN-YMD8
         CALL "SKYDTCKB" USING  LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  4     TO  ERR-FLG
             END-IF
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-JYUST
             MOVE  "R"       TO  EDIT-OPTION OF DSP-JYUST
             GO TO    BODY-CHK-EXIT
             END-IF
     END-IF.
*終了日付
     IF  DSP-JYUED NOT NUMERIC
     OR  DSP-JYUED = ZERO
     OR  DSP-JYUED = 99999999
         MOVE   99999999    TO  DSP-JYUED
     ELSE
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-JYUED    TO  LINK-IN-YMD8
         CALL "SKYDTCKB" USING  LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  4     TO  ERR-FLG
             END-IF
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-JYUED
             MOVE  "R"       TO  EDIT-OPTION OF DSP-JYUED
             GO TO    BODY-CHK-EXIT
             END-IF
     END-IF.
*帳票出力区分未入力はエラー
     IF     DSP-SYUKBN = SPACE
         OR DSP-SYUKBN = ZERO
         MOVE  1                 TO  ERR-FLG
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYUKBN
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYUKBN
         MOVE  SPACE             TO  DSP-SYUKBN
         GO TO  BODY-CHK-EXIT
     END-IF.
*帳票出力区分＝１，２以外はエラー
     IF  DSP-SYUKBN = 1 OR  2
         CONTINUE
     ELSE
         MOVE  1                 TO  ERR-FLG
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYUKBN
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYUKBN
         MOVE  SPACE             TO  DSP-SYUKBN
     END-IF.
*
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目（手書き）  入力( PSW = 3 )
****************************************************************
 DSP-BODY2-SEC          SECTION.
*
     MOVE     "DSP-BODY2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  BODY2-CHK-SEC
                IF  ERR-FLG = ZERO
                    MOVE  "4"    TO   PSW
                ELSE
                    MOVE  "3"    TO   PSW
                END-IF
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
*             明細項目チェック                                 *
****************************************************************
 BODY2-CHK-SEC         SECTION.
*
     MOVE   "BODY2-CHK-SEC"      TO  S-NAME.
*
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SIRST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SIRST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SIRED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SIRED.
*開始日付
     IF  DSP-SIRST NOT NUMERIC
     OR  DSP-SIRST = ZERO
         MOVE   ZERO        TO  DSP-SIRST
     ELSE
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-SIRST    TO  LINK-IN-YMD8
         CALL "SKYDTCKB" USING  LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  4     TO  ERR-FLG
             END-IF
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-SIRST
             MOVE  "R"       TO  EDIT-OPTION OF DSP-SIRST
             GO TO    BODY-CHK-EXIT
             END-IF
     END-IF.
*終了日付
     IF  DSP-SIRED NOT NUMERIC
     OR  DSP-SIRED = ZERO
     OR  DSP-SIRED = 99999999
         MOVE   99999999    TO  DSP-SIRED
     ELSE
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-SIRED    TO  LINK-IN-YMD8
         CALL "SKYDTCKB" USING  LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  4     TO  ERR-FLG
             END-IF
             MOVE  "C"       TO  EDIT-CURSOR OF DSP-SIRED
             MOVE  "R"       TO  EDIT-OPTION OF DSP-SIRED
             GO TO    BODY-CHK-EXIT
             END-IF
     END-IF.
*帳票出力区分未入力はエラー
     IF     DSP-SYUKBN = SPACE
         OR DSP-SYUKBN = ZERO
         MOVE  1                 TO  ERR-FLG
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYUKBN
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYUKBN
         MOVE  SPACE             TO  DSP-SYUKBN
         GO TO  BODY2-CHK-EXIT
     END-IF.
*帳票出力区分＝１，２以外はエラー
     IF  DSP-SYUKBN = 1 OR  2
         CONTINUE
     ELSE
         MOVE  1                 TO  ERR-FLG
         MOVE  "C"               TO  EDIT-CURSOR OF DSP-SYUKBN
         MOVE  "R"               TO  EDIT-OPTION OF DSP-SYUKBN
         MOVE  SPACE             TO  DSP-SYUKBN
     END-IF.
*
 BODY2-CHK-EXIT.
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
         PERFORM  SET-SEC
         MOVE  "END"        TO  END-FLG
*終了
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG
         MOVE  "4010"       TO  PROGRAM-STATUS
*項目戻し
       WHEN  "F006"
         IF  DSP-KBN = "1"  *> 受信日
             MOVE  "2"      TO  PSW
         ELSE               *> 仕入計上日
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
*    パラメータセット処理                                      *
****************************************************************
 SET-SEC         SECTION.
*
     IF    DSP-KBN  =  "1"
           MOVE  DSP-KBN       TO   PARA-KBN
           MOVE  DSP-JYUST     TO   PARA-JYUSIN-ST
           MOVE  DSP-JYUED     TO   PARA-JYUSIN-ED
           MOVE  ZERO          TO   PARA-SIIRED-ST
           MOVE  99999999      TO   PARA-SIIRED-ED
           MOVE  DSP-SYUKBN    TO   PARA-LIST-KBN
     ELSE
           MOVE  DSP-KBN       TO   PARA-KBN
           MOVE  ZERO          TO   PARA-JYUSIN-ST
           MOVE  99999999      TO   PARA-JYUSIN-ED
           MOVE  DSP-SIRST     TO   PARA-SIIRED-ST
           MOVE  DSP-SIRST     TO   PARA-SIIRED-ED
           MOVE  DSP-SYUKBN    TO   PARA-LIST-KBN
     END-IF.
*
 SET-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37821.
*ＰＧＩＤ
     MOVE  "SSY3782I"       TO  DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-KBN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KBN.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-JYUST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SIRST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SIRED.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3782I   END PROGRAM  >>******************

```
