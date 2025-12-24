***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：ナフコＥＤＩ管理番号リスト指示     *
*    作成日／更新日　：2010/10/05                         *
*    作成者／更新者　：ＮＡＶ飯田                         *
*    処理概要　　　　：                                   *
*      ナフコＥＤＩ管理番号リストの処理条件を入力し       *
*      チェックＯＫなら条件をパラメータに出力する。       *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3777N.
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

*管理番号ファイル
     SELECT  NFKANRF   ASSIGN    TO        NFKANRL3
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KNR-F01
                                           KNR-F031
                       FILE      STATUS    KNR-ST.
*担当者マスタ
     SELECT  HTANMS   ASSIGN    TO         TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01
                                           TAN-F02
                       FILE      STATUS    TAN-ST.

****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY37771  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.

****************************************************************
*    FILE = 管理番号ファイル                                   *
****************************************************************
 FD  NFKANRF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFKANRF   OF   XFDLIB
                       JOINING   KNR       AS   PREFIX.

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
     03  KNR-ST                   PIC  X(02).
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
     03  FG-NFKANRF-END           PIC  X(03)  VALUE  SPACE.
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
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"対象データが存在しません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE
             NC"　".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE
             NC"　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG10.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG11.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG12.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG13.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
     03  ERR-MSG14.
         05  FILLER              PIC   N(25)
             VALUE NC"　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  14  PIC   N(25).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  KNR-ERR           PIC N(20) VALUE
                        NC"管理番号ファイルエラー".
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
 01  PAR-BMNCD             PIC  X(04).
 01  PAR-TANCD             PIC  X(02).
* 出力パラメータ
 01  PAR-DTST              PIC  9(08).
 01  PAR-DTED              PIC  9(08).
 01  PAR-SYTANCD           PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  PAR-BMNCD
                                         PAR-TANCD
                                         PAR-DTST
                                         PAR-DTED
                                         PAR-SYTANCD.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KNR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFKANRF.
     DISPLAY     KNR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KNR-ST    UPON      CONS.
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
     OPEN  INPUT NFKANRF.
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

         MOVE  SPACE             TO  DSP-TANNM
         IF  DSP-TANCD NOT = SPACE
             MOVE  PAR-BMNCD     TO  TAN-F01
             MOVE  DSP-TANCD     TO  TAN-F02
             PERFORM  RD-HTANMS-SEC
             IF  FG-HTANMS-INV = ZERO
                 MOVE  TAN-F03   TO  DSP-TANNM
             END-IF
         END-IF
*確認
       WHEN  "2"
         MOVE  PF-MSG-R(2)       TO  DSP-PFGAID
       WHEN  OTHER
         MOVE  SPACE             TO  DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY37771"          TO   DSP-FMT.
     WRITE    DSP-FSY37771.
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
*明細項目
         WHEN   "1"
                MOVE    "MAIN"    TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSY37771"           TO   DSP-FMT.
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

* データ作成日
     IF      EDIT-STATUS OF DSP-DTST = SPACE
         AND DSP-DTST NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-DTST     TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DTST
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DTST
             GO TO  BODY-CHK-EXIT
         END-IF
     END-IF.

     IF      EDIT-STATUS OF DSP-DTED = SPACE
         AND DSP-DTED NOT = ZERO
         MOVE  "2"          TO  LINK-IN-KBN
         MOVE  DSP-DTED     TO  LINK-IN-YMD8
         CALL  "SKYDTCKB" USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD
         IF  LINK-OUT-RET NOT = ZERO
             IF  ERR-FLG = ZERO
                 MOVE  1    TO  ERR-FLG
             END-IF
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DTED
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DTED
             GO TO  BODY-CHK-EXIT
         END-IF

     END-IF.

     IF     (    EDIT-STATUS OF DSP-DTST = SPACE
             AND DSP-DTST NOT = ZERO)
        AND (    EDIT-STATUS OF DSP-DTED = SPACE
             AND DSP-DTED NOT = ZERO)
        AND DSP-DTST > DSP-DTED
        IF  ERR-FLG = ZERO
            MOVE  1         TO  ERR-FLG
        END-IF
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-DTST
        MOVE  "R"           TO  EDIT-OPTION OF DSP-DTST
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-DTED
        MOVE  "R"           TO  EDIT-OPTION OF DSP-DTED
        GO TO  BODY-CHK-EXIT
     END-IF.


* 担当者コード
* 担当者名
     MOVE  SPACE            TO  DSP-TANNM.
     IF  DSP-TANCD NOT = SPACE
         MOVE  PAR-BMNCD    TO  TAN-F01
         MOVE  DSP-TANCD    TO  TAN-F02
         PERFORM  RD-HTANMS-SEC
         IF  FG-HTANMS-INV = 1
             MOVE  1        TO  ERR-FLG
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-TANCD
             MOVE  "R"      TO  EDIT-OPTION OF DSP-TANCD
             GO TO  BODY-CHK-EXIT
         ELSE
             MOVE  TAN-F03  TO  DSP-TANNM
         END-IF

     END-IF.

     IF  ERR-FLG = ZERO
         MOVE  LOW-VALUE    TO  FG-NFKANRF-END
         PERFORM  RD-NFKANRF-SEC
         IF  FG-NFKANRF-END = "END"
             IF  ERR-FLG = ZERO
                 MOVE  2    TO  ERR-FLG
             END-IF

             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DTST
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DTST
             MOVE  "C"      TO  EDIT-CURSOR OF DSP-DTED
             MOVE  "R"      TO  EDIT-OPTION OF DSP-DTED
             GO TO  BODY-CHK-EXIT
         END-IF

     END-IF.

     MOVE  "2"                   TO  PSW.

 BODY-CHK-EXIT.
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

 RD-HTANMS-EXIT.
     EXIT.

****************************************************************
*    管理番号ファイル２読込み　                                *
****************************************************************
 RD-NFKANRF-SEC         SECTION.

     IF  FG-NFKANRF-END = LOW-VALUE
         MOVE   SPACE    TO     KNR-REC
         INITIALIZE             KNR-REC
         MOVE   "1"      TO     KNR-F01
         MOVE   DSP-DTST TO     KNR-F031
         START  NFKANRF  KEY >= KNR-F01
                                KNR-F031
           INVALID KEY
              MOVE  "END"   TO  FG-NFKANRF-END
              GO TO  RD-NFKANRF-EXIT
         END-START

         MOVE  SPACE        TO  FG-NFKANRF-END

     END-IF.

     READ  NFKANRF
       AT  END
         MOVE  "END"        TO  FG-NFKANRF-END
         GO TO  RD-NFKANRF-EXIT
     END-READ.

     IF     (   DSP-DTST NOT NUMERIC
             OR DSP-DTST = ZERO)
        AND (   DSP-DTED NOT NUMERIC
             OR DSP-DTED = ZERO)
        CONTINUE
     ELSE
        IF      KNR-F031 >= DSP-DTST
            AND KNR-F031 <= DSP-DTED
            CONTINUE
        ELSE
            GO TO  RD-NFKANRF-SEC
        END-IF
     END-IF.

 RD-NFKANRF-EXIT.
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
     IF  DSP-DTST NOT NUMERIC
         MOVE  ZERO         TO  PAR-DTST
     ELSE
         MOVE DSP-DTST      TO  PAR-DTST
     END-IF.

     IF  DSP-DTED NOT NUMERIC
         MOVE  ALL "9"      TO  PAR-DTED
     ELSE
         MOVE DSP-DTED      TO  PAR-DTED
     END-IF.
     MOVE DSP-TANCD         TO  PAR-SYTANCD.
 PARA-OT-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE  "INIT-DSP-SEC"   TO  S-NAME.
*画面の初期化
     MOVE  SPACE            TO  DSP-FSY37771.
*ＰＧＩＤ
     MOVE  "SSY3777N"       TO  DSP-PGID.
*システム日付転送
     MOVE  SYS-DATE         TO  DSP-SDATE.
*システム時間転送
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
*担当者コード
     MOVE  PAR-TANCD        TO  DSP-TANCD.
*リバース，カーソルパーク解除
***  メッセージＮＯ
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DTST.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DTST.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-DTED.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DTED.
     MOVE  "M"              TO  EDIT-OPTION OF DSP-TANCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANCD.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE  NFKANRF.
     CLOSE  HTANMS.
     CLOSE  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3777N   END PROGRAM  >>******************
