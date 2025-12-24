***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：手書出荷指示データ取込             *
*    作成日／更新日　：2010/10/08                         *
*    作成者／更新者　：ＮＡＶ飯田                         *
*    処理概要　　　　：                                   *
*      手書出荷指示データ取込処理の処理条件を入力し       *
*      チェックＯＫなら条件をパラメータに出力する。       *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3762I.
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

*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN             TO  SHTDENL1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  SEQUENTIAL
       RECORD KEY         IS  URI-F01  *> 取引先コード
                              URI-F02  *> 伝票番号
                              URI-F04  *> 相殺区分
                              URI-F051 *> 伝区コード
                              URI-F03  *> 行番号
       FILE STATUS        IS  URI-ST.

*作場マスタ
     SELECT  SAKUBAF
       ASSIGN             TO  SAKUBAL1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  RANDOM
       RECORD KEY         IS  SKB-F01 *> 作場コード
       FILE STATUS        IS  SKB-ST.

*担当者マスタ
     SELECT  HTANMS
       ASSIGN             TO  TANMS1
       ORGANIZATION       IS  INDEXED
       ACCESS MODE        IS  RANDOM
       RECORD KEY         IS  TAN-F01 *> 部門コード
                              TAN-F02 *> 担当者コード
       FILE STATUS        IS  TAN-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37621  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   URI       AS   PREFIX.

****************************************************************
*    FILE = 作場マスタ                                       *
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF    OF   XFDLIB
                       JOINING   SKB       AS   PREFIX.

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
     03  DSP-ST                   PIC  X(02).
     03  URI-ST                   PIC  X(02).
     03  SKB-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
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
     03  FG-SHTDENF-END           PIC  X(03)  VALUE  SPACE.
     03  FG-SAKUBAF-INV           PIC  9(01)  VALUE  ZERO.
     03  FG-HTANMS-INV            PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
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
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(25)  VALUE
         NC"正しい値を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)  VALUE
         NC"対象データが存在しません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)  VALUE
         NC"出荷場所を入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)  VALUE
         NC"　".
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
     03  URI-ERR           PIC N(20) VALUE
                        NC"売上伝票ファイルエラー".
     03  SKB-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
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
 01  PAR-BMNCD              PIC  X(04). *> 部門コード
 01  PAR-TANCD              PIC  X(02). *> 担当者コード
* 出力パラメータ
 01  PAR-SKBCD              PIC  X(02). *> 作場コード
 01  PAR-NONYYMD            PIC  9(08). *> 納品日
 01  PAR-STDENNO            PIC  9(09). *> 開始伝票ＮＯ
 01  PAR-EDDENNO            PIC  9(09). *> 終了伝票ＮＯ
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BMNCD
                                         PAR-TANCD
                                         PAR-SKBCD
                                         PAR-NONYYMD
                                         PAR-STDENNO
                                         PAR-EDDENNO.
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
 SKB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SKB-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SKB-ST    UPON      CONS.
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
     OPEN  INPUT SHTDENF.
     OPEN  INPUT SAKUBAF.
     OPEN  INPUT HTANMS.
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
*明細項目入力
         WHEN      "1"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
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
                MOVE    "4010"   TO  PROGRAM-STATUS
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
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.

     PERFORM  SDATE-GET-SEC.
*システム日付転送
     MOVE  SYS-DATE               TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)           TO  DSP-STIME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE  PSW
*明細項目
       WHEN  "1"
         MOVE  PF-MSG-R(1)       TO  DSP-PFGAID
* 担当者名
         MOVE  SPACE             TO  DSP-TANNM
         MOVE  PAR-BMNCD         TO  TAN-F01
         MOVE  PAR-TANCD         TO  TAN-F02
         PERFORM  RD-HTANMS-SEC
         IF  FG-HTANMS-INV = ZERO
             MOVE  TAN-F03       TO  DSP-TANNM
         END-IF
* 出荷場所名
         MOVE  SPACE             TO  DSP-SAKBNM
         IF  DSP-SAKBCD NOT = SPACE
             MOVE  DSP-SAKBCD    TO  SKB-F01
             PERFORM  RD-SAKUBAF-SEC
             IF  FG-SAKUBAF-INV = ZERO
                 MOVE  SKB-F02   TO  DSP-SAKBNM
             END-IF
         END-IF

         PERFORM  DSP-ZERO-SPACE-SEC

*確認
       WHEN  "2"
         PERFORM  DSP-ZERO-SPACE-SEC

         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE             TO  DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY37621"          TO   DSP-FMT.
     WRITE    DSP-FSY37621.
*
 DSP-WRITE-EXIT.
     EXIT.

****************************************************************
*    作場マスタ検索                                          *
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
*    担当者マスタ検索                                          *
****************************************************************
 RD-HTANMS-SEC          SECTION.
     READ  HTANMS
       INVALID
         MOVE  1                 TO  FG-HTANMS-INV
       NOT INVALID
         MOVE  ZERO              TO  FG-HTANMS-INV
     END-READ.

 RD-SAKUBAF-EXIT.
     EXIT.

****************************************************************
*             ゼロを空白表示する                               *
****************************************************************
 DSP-ZERO-SPACE-SEC          SECTION.
     IF  DSP-NOUYMD = ZERO
         MOVE  SPACE        TO  DSP-NOUYMD (1:8)
     END-IF.

     IF  DSP-SDENNO = ZERO
         MOVE  SPACE        TO  DSP-SDENNO (1:9)
     END-IF.

     IF  DSP-EDENNO = ZERO
         MOVE  SPACE        TO  DSP-EDENNO (1:9)
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
*明細項目
         WHEN   "1"
                MOVE    "MAIN"    TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37621"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                                 *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE   "BODY-CHK-SEC"       TO  S-NAME.

     MOVE  SPACE          TO  EDIT-CURSOR OF DSP-SAKBCD.
     MOVE  "M"            TO  EDIT-OPTION OF DSP-SAKBCD.
     MOVE  SPACE          TO  EDIT-CURSOR OF DSP-NOUYMD.
     MOVE  "M"            TO  EDIT-OPTION OF DSP-NOUYMD.
     MOVE  SPACE          TO  EDIT-CURSOR OF DSP-SDENNO.
     MOVE  "M"            TO  EDIT-OPTION OF DSP-SDENNO.
     MOVE  SPACE          TO  EDIT-CURSOR OF DSP-EDENNO.
     MOVE  "M"            TO  EDIT-OPTION OF DSP-EDENNO.

* 担当者コード

* 出荷場所
     MOVE  SPACE            TO  DSP-SAKBNM.
     IF  DSP-SAKBCD = SPACE
         IF  ERR-FLG = ZERO
             MOVE  3        TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR OF DSP-SAKBCD
         MOVE  "R"          TO  EDIT-OPTION OF DSP-SAKBCD
         GO TO  BODY-CHK-EXIT
     ELSE
         MOVE  DSP-SAKBCD    TO  SKB-F01
         PERFORM  RD-SAKUBAF-SEC
         IF  FG-SAKUBAF-INV = 1
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SAKBCD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SAKBCD
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  SKB-F02  TO  DSP-SAKBNM
         END-IF
     END-IF.

* 納品日
     IF      EDIT-STATUS OF DSP-NOUYMD = SPACE
         AND DSP-NOUYMD NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-NOUYMD   TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-NOUYMD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-NOUYMD
             GO TO  BODY-CHK-EXIT
         END-IF
     END-IF.

* 開始伝票番号

* 終了伝票番号

     IF     (    EDIT-STATUS OF DSP-SDENNO = SPACE
             AND DSP-SDENNO NOT = ZERO)
        AND (    EDIT-STATUS OF DSP-EDENNO = SPACE
             AND DSP-EDENNO NOT = ZERO)
        AND DSP-SDENNO > DSP-EDENNO
        IF  ERR-FLG = ZERO
            MOVE  1         TO  ERR-FLG
        END-IF
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-SDENNO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SDENNO
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-EDENNO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-EDENNO
        GO TO  BODY-CHK-EXIT
     END-IF.


     IF  ERR-FLG = ZERO
         MOVE  LOW-VALUE    TO  FG-SHTDENF-END
*********MOVE  137607       TO  URI-F01  *> 取引先コード
         MOVE  999977       TO  URI-F01  *> 取引先コード
         MOVE  ZERO         TO  URI-F02  *> 伝票番号
         MOVE  ZERO         TO  URI-F04  *> 相殺区分
         MOVE  ZERO         TO  URI-F051 *> 伝区コード
         MOVE  ZERO         TO  URI-F03  *> 行番号
         PERFORM  RD-SHTDENF-SEC
         IF  FG-SHTDENF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  2    TO  ERR-FLG
             END-IF

             MOVE  "C"      TO  EDIT-CURSOR OF DSP-SAKBCD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-SAKBCD
             GO TO  BODY-CHK-EXIT
         END-IF

     END-IF.

     MOVE  "2"                   TO  PSW.

 BODY-CHK-EXIT.
     EXIT.

****************************************************************
*    売上伝票ファイル２読込み　                                *
****************************************************************
 RD-SHTDENF-SEC         SECTION.

     IF  FG-SHTDENF-END = LOW-VALUE
         START  SHTDENF  KEY >= URI-F01  *> 取引先コード
                                URI-F02  *> 伝票番号
                                URI-F04  *> 相殺区分
                                URI-F051 *> 伝区コード
                                URI-F03  *> 行番号
           INVALID KEY
              MOVE  "END"   TO  FG-SHTDENF-END
              GO TO  RD-SHTDENF-EXIT
         END-START

         MOVE  SPACE        TO  FG-SHTDENF-END

     END-IF.

     READ  SHTDENF
       AT  END
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-READ.

*****IF  URI-F01 > 137607    *> 取引先コード
     IF  URI-F01 > 999977    *> 取引先コード
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.

* 出荷場所（作場ＣＤ）
     IF  URI-F08 NOT = DSP-SAKBCD
         GO TO  RD-SHTDENF-SEC
     END-IF.
* 納品日
     IF     DSP-NOUYMD NOT NUMERIC
         OR DSP-NOUYMD = ZERO
         CONTINUE
     ELSE
         IF  URI-F112 NOT = DSP-NOUYMD
             GO TO  RD-SHTDENF-SEC
         END-IF
     END-IF.
* 伝票ＮＯ範囲
     EVALUATE  TRUE
       WHEN      (   DSP-SDENNO NOT NUMERIC
                  OR DSP-SDENNO = ZERO)
             AND (   DSP-EDENNO NOT NUMERIC
                  OR DSP-EDENNO = ZERO)
        CONTINUE
       WHEN      (   DSP-SDENNO NOT NUMERIC
                  OR DSP-SDENNO = ZERO)
         IF  URI-F02 <= DSP-EDENNO
             CONTINUE
         ELSE
             GO TO  RD-SHTDENF-SEC
         END-IF

       WHEN      (   DSP-EDENNO NOT NUMERIC
                  OR DSP-EDENNO = ZERO)
         IF  URI-F02 >= DSP-SDENNO
             CONTINUE
         ELSE
             GO TO  RD-SHTDENF-SEC
         END-IF

       WHEN  OTHER
         IF      URI-F02 >= DSP-SDENNO
             AND URI-F02 <= DSP-EDENNO
             CONTINUE
         ELSE
             GO TO  RD-SHTDENF-SEC
         END-IF
     END-EVALUATE.

 RD-SHTDENF-EXIT.
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
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM  PARA-OT-SEC
                MOVE    "END"    TO  END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO  END-FLG
                MOVE    "4010"   TO  PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE  "1"        TO  PSW
*取消
         WHEN   "F004"
                MOVE  "1"        TO  PSW
                PERFORM   INIT-DSP-SEC

         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    パラメータ出力処理                                        *
****************************************************************
 PARA-OT-SEC          SECTION.
     MOVE  DSP-SAKBCD       TO  PAR-SKBCD.

     MOVE  DSP-SAKBCD       TO  PAR-SKBCD.

     IF  DSP-NOUYMD  NOT NUMERIC
         MOVE  ZERO         TO  PAR-NONYYMD
     ELSE
         MOVE  DSP-NOUYMD   TO  PAR-NONYYMD
     END-IF.

     IF  DSP-SDENNO  NOT NUMERIC
         MOVE  ZERO         TO  PAR-STDENNO
     ELSE
         MOVE  DSP-SDENNO   TO  PAR-STDENNO
     END-IF.

     IF     DSP-EDENNO  NOT NUMERIC
         OR DSP-EDENNO = ZERO
         MOVE  ALL "9"      TO  PAR-EDDENNO
     ELSE
         MOVE  DSP-EDENNO   TO  PAR-EDDENNO
     END-IF.

 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37621.
*ＰＧＩＤ
     MOVE  "SSY3762I"       TO  DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*担当者コード
     MOVE  PAR-TANCD        TO  DSP-TANCD.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SAKBCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKBCD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-NOUYMD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NOUYMD.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-SDENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SDENNO.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-EDENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-EDENNO.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  SHTDENF.
     CLOSE  SAKUBAF.
     CLOSE  HTANMS.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3762I   END PROGRAM  >>******************
