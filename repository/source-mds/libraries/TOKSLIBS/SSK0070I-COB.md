# SSK0070I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0070I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス　　　            *
*    モジュール名　　　　：　売上データリスト発行指示　　      *
*    作成日／更新日　　　：　2014/03/13                        *
*    作成者／更新者　　　：　NAV/MIURA                         *
*    処理概要　　　　　　：　受領書＆ＣＳＶ出力の指示を画面より*
*                        ：　行なう（ＳＳＫ００２０Ｉ）流用　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSK0070I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         14/03/06.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSK00701  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
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
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  TEN-FLG                  PIC  9(01)  VALUE  ZERO.
     03  DENN-FLG                 PIC  9(01)  VALUE  ZERO.
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
     03  SYS-DATE                 PIC  9(08).
*
*システム日付／時刻編集　　
 01  WK-DATEX.
     03  WK-YYYY                  PIC   9(04)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  "/".
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  "/".
     03  WK-DD                    PIC   9(02)  VALUE  ZERO.
 01  WK-TIMEX.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  ":".
     03  WK-MN                    PIC   9(02)  VALUE  ZERO.
     03  FILLER                   PIC   X(01)  VALUE  ":".
     03  WK-SS                    PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*項目退避ワーク
 01  WK-KOMOKU.
     03  WK-SKBN                  PIC   X(01)  VALUE  SPACE.
     03  WK-DKBN                  PIC   X(01)  VALUE  SPACE.
     03  WK-TENF                  PIC   9(05)  VALUE  ZERO.
     03  WK-TENT                  PIC   9(05)  VALUE  ZERO.
     03  WK-DENNF                 PIC   9(09)  VALUE  ZERO.
     03  WK-DENNT                 PIC   9(09)  VALUE  ZERO.
     03  WK-SKBF                  PIC   X(02)  VALUE  SPACE.
     03  WK-SKBT                  PIC   X(02)  VALUE  SPACE.
     03  WK-DENKF                 PIC   X(02)  VALUE  SPACE.
     03  WK-DENKT                 PIC   X(02)  VALUE  SPACE.
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
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出力区分を入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付区分を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付を入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付範囲を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい店舗範囲を入力して下さい。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい伝票番号範囲を入力して下さい。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出荷場所範囲を入力して下さい。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい伝票区分範囲を入力して下さい。".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD8         PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-BMNCD              PIC  X(04).
 01  LINK-TANCD              PIC  X(02).
 01  LINK-SKBN               PIC  X(01).
 01  LINK-DKBN               PIC  X(01).
 01  LINK-DFROM              PIC  9(08).
 01  LINK-DTO                PIC  9(08).
 01  LINK-KKBN               PIC  X(01).
 01  LINK-TANFROM            PIC  X(02).
 01  LINK-TANFTO             PIC  X(02).
 01  LINK-DENK1              PIC  X(02).
 01  LINK-DENK2              PIC  X(02).
 01  LINK-DENK3              PIC  X(02).
 01  LINK-DENK4              PIC  X(02).
 01  LINK-DENK5              PIC  X(02).
 01  LINK-TENFROM            PIC  9(05).
 01  LINK-TENTO              PIC  9(05).
 01  LINK-DENNOFROM          PIC  9(09).
 01  LINK-DENNOTO            PIC  9(09).
 01  LINK-SBSFROM            PIC  X(02).
 01  LINK-SBSTO              PIC  X(02).
 01  LINK-DENKFROM           PIC  X(02).
 01  LINK-DENKTO             PIC  X(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-BMNCD
                                         LINK-TANCD
                                         LINK-SKBN
                                         LINK-DKBN
                                         LINK-DFROM
                                         LINK-DTO
                                         LINK-KKBN
                                         LINK-TANFROM
                                         LINK-TANFTO
                                         LINK-DENK1
                                         LINK-DENK2
                                         LINK-DENK3
                                         LINK-DENK4
                                         LINK-DENK5
                                         LINK-TENFROM
                                         LINK-TENTO
                                         LINK-DENNOFROM
                                         LINK-DENNOTO
                                         LINK-SBSFROM
                                         LINK-SBSTO
                                         LINK-DENKFROM
                                         LINK-DENKTO.
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
     MOVE     "PROCESS START"     TO   S-NAME.
***
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE    SYS-DATE(1:4)        TO   WK-YYYY.
     MOVE    SYS-DATE(5:2)        TO   WK-MM.
     MOVE    SYS-DATE(7:2)        TO   WK-DD.
     MOVE    WK-TIME(1:2)         TO   WK-HH.
     MOVE    WK-TIME(3:2)         TO   WK-MN.
     MOVE    WK-TIME(5:2)         TO   WK-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
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
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*出力区分入力
         WHEN      "1"  PERFORM   DSP-PAR1-SEC
*日付区分入力
         WHEN      "2"  PERFORM   DSP-PAR2-SEC
*日付入力
         WHEN      "H"  PERFORM   DSP-PAR21-SEC
         WHEN      "N"  PERFORM   DSP-PAR21-SEC
*店舗・伝票番号・出荷場所
         WHEN      "3"  PERFORM   DSP-PAR3-SEC
*伝区入力　　
         WHEN      "4"  PERFORM   DSP-PAR4-SEC
*確認入力
         WHEN      "5"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PAR1-SEC         SECTION.
     MOVE     "DSP-PAR1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK1-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR1-SEC
     END-EVALUATE.
*
 DSP-PAR1-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 2 )                2.1       *
****************************************************************
 DSP-PAR2-SEC         SECTION.
     MOVE     "DSP-PAR2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK2-SEC
                EVALUATE   DSP-DKBN
                  WHEN   "1"
                    MOVE  "H"  TO    PSW
                  WHEN   "2"
                    MOVE  "N"  TO    PSW
                END-EVALUATE
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR2-SEC
     END-EVALUATE.
*
 DSP-PAR2-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 21)                2.1       *
****************************************************************
 DSP-PAR21-SEC         SECTION.
     MOVE     "DSP-PAR21-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK21-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR21-SEC
     END-EVALUATE.
*
 DSP-PAR21-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 3 )                2.1       *
****************************************************************
 DSP-PAR3-SEC         SECTION.
     MOVE     "DSP-PAR3-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK3-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN   "F006"
                EVALUATE   DSP-DKBN
                  WHEN   "1"
                    MOVE  "H"  TO    PSW
                  WHEN   "2"
                    MOVE  "N"  TO    PSW
                END-EVALUATE
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR3-SEC
     END-EVALUATE.
*
 DSP-PAR3-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 4 )                2.1       *
****************************************************************
 DSP-PAR4-SEC         SECTION.
     MOVE     "DSP-PAR4-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK4-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN   "F006"
                MOVE    "3"  TO    PSW
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR4-SEC
     END-EVALUATE.
*
 DSP-PAR4-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック1                      2.1.1     *
****************************************************************
 PARA-CHK1-SEC             SECTION.
     MOVE     "PARA-CHK1-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*出力区分チェック
     IF       DSP-SKBN   =  "1"  OR  "2"
              CONTINUE
     ELSE
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN
              GO             TO   PARA-CHK1-EXIT
     END-IF.
     MOVE    "2"       TO    PSW.
*
 PARA-CHK1-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック2                      2.1.2     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
*属性を初期化する
*    PERFORM  DSP-SYOKI-SEC.
     PERFORM  INIT-DSP-SEC2.
*日付区分チェック
     IF       DSP-DKBN   =  "1"  OR  "2"
              CONTINUE
     ELSE
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DKBN
              GO             TO   PARA-CHK2-EXIT
     END-IF.
 PARA-CHK2-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック21                     2.1.3     *
****************************************************************
 PARA-CHK21-SEC             SECTION.
     MOVE     "PARA-CHK21-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
     EVALUATE   DSP-DKBN
       WHEN   "1"
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-HFROM  TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  3         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-HFROM
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-HFROM
                   GO   TO   PARA-CHK21-EXIT
           END-IF
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-HTO    TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  3         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-HTO
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-HTO
                GO   TO   PARA-CHK21-EXIT
           END-IF
*日付大小チェック
           IF       DSP-HFROM  >  DSP-HTO
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-HFROM
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-HTO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-HFROM
              GO             TO   PARA-CHK21-EXIT
           END-IF
       WHEN   "2"
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-NFROM  TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  3         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-NFROM
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-NFROM
                   GO   TO   PARA-CHK21-EXIT
           END-IF
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-NTO    TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  3         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-NTO
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-NTO
                   GO   TO   PARA-CHK21-EXIT
           END-IF
           IF       DSP-NFROM  >  DSP-NTO
              MOVE      4    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NFROM
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NTO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-NFROM
              GO             TO   PARA-CHK21-EXIT
           END-IF
     END-EVALUATE.
     MOVE    "3"       TO    PSW.
*
 PARA-CHK21-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック3                      2.1.3     *
****************************************************************
 PARA-CHK3-SEC             SECTION.
     MOVE     "PARA-CHK3-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
     MOVE    "4"       TO    PSW.
*大小チェック
*数値初期値セット
     IF       DSP-DENNF  =  ZERO
     OR       DSP-DENNF  NOT  NUMERIC
              MOVE  ZERO     TO   DSP-DENNF
     END-IF.
     IF       DSP-DENNT  =  ZERO
     OR       DSP-DENNT  NOT  NUMERIC
              MOVE  999999999   TO   DSP-DENNT
     END-IF.
     IF       DSP-TENF  =  ZERO
     OR       DSP-TENF  NOT  NUMERIC
              MOVE  ZERO     TO   DSP-TENF
     END-IF.
     IF       DSP-TENT  =  ZERO
     OR       DSP-TENT  NOT  NUMERIC
              MOVE  99999   TO   DSP-TENT
     END-IF.
     IF  DSP-SKBF  >  DSP-SKBT
         MOVE      7    TO   ERR-FLG
         MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SKBF
         MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SKBT
         MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SKBF
         MOVE     "3"   TO    PSW
     END-IF.
     IF  DSP-DENNF IS  NUMERIC
         IF  DSP-DENNF  >  DSP-DENNT
             MOVE      6    TO   ERR-FLG
             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-DENNF
             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-DENNT
             MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-DENNF
             MOVE     "3"   TO    PSW
         END-IF
     END-IF.
     IF  DSP-TENF  IS  NUMERIC
         IF  DSP-TENF   >  DSP-TENT
             MOVE      5    TO   ERR-FLG
             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-TENF
             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-TENT
             MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-TENF
             MOVE     "3"   TO    PSW
         END-IF
     END-IF.
*
 PARA-CHK3-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック4                      2.1.3     *
****************************************************************
 PARA-CHK4-SEC             SECTION.
     MOVE     "PARA-CHK4-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*大小チェック
     IF  DSP-DENKF >  DSP-DENKT
         MOVE      8    TO   ERR-FLG
         MOVE     "R"   TO   EDIT-OPTION  OF   DSP-DENKF
         MOVE     "R"   TO   EDIT-OPTION  OF   DSP-DENKT
         MOVE     "C"   TO   EDIT-CURSOR  OF   DSP-DENKF
         GO    TO     PARA-CHK4-EXIT
     END-IF.
     MOVE     "5"   TO    PSW.
*
 PARA-CHK4-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 5 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    DSP-SKBN      TO   LINK-SKBN
*               MOVE    DSP-DKBN      TO   LINK-DKBN
                IF      DSP-DKBN      =    "1"
                        MOVE    "4"   TO   LINK-DKBN
                ELSE
                        MOVE    "3"   TO   LINK-DKBN
                END-IF
                EVALUATE   DSP-DKBN
                  WHEN   "1"
                    MOVE    DSP-HFROM     TO   LINK-DFROM
                    MOVE    DSP-HTO       TO   LINK-DTO
                  WHEN   "2"
                    MOVE    DSP-NFROM     TO   LINK-DFROM
                    MOVE    DSP-NTO       TO   LINK-DTO
                END-EVALUATE
                MOVE    DSP-TENF      TO   LINK-TENFROM
                MOVE    DSP-TENT      TO   LINK-TENTO
                MOVE    DSP-DENNF     TO   LINK-DENNOFROM
                MOVE    DSP-DENNT     TO   LINK-DENNOTO
                MOVE    DSP-SKBF      TO   LINK-SBSFROM
                MOVE    DSP-SKBT      TO   LINK-SBSTO
                MOVE    DSP-DENKF     TO   LINK-DENKFROM
                MOVE    DSP-DENKT     TO   LINK-DENKTO
                MOVE    "END"         TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "4"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "H"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "N"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "5"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "SSK0070I"          TO   DSP-PGID.
     MOVE    "FSK00701"          TO   DSP-FORMID.
     MOVE    "FSK00701"          TO   DSP-FMT.
     WRITE    DSP-FSK00701.
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
     EVALUATE   PSW
*パラメタ項目
         WHEN   "1"
                MOVE    "PARGR1"  TO   DSP-GRP
*パラメタ項目
         WHEN   "2"
                MOVE    "PARGR2"  TO   DSP-GRP
*パラメタ項目
         WHEN   "N"
                MOVE    "PARGRN"  TO   DSP-GRP
         WHEN   "H"
                MOVE    "PARGRH"  TO   DSP-GRP
*パラメタ項目
         WHEN   "3"
                MOVE    "PARGR3"  TO   DSP-GRP
*パラメタ項目
         WHEN   "4"
                MOVE    "PARGR4"  TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "SSK0070I"           TO   DSP-PGID.
     MOVE    "FSK00701"           TO   DSP-FORMID.
     MOVE    "FSK00701"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSK00701.
*システム日付転送
*    MOVE    SYS-DATE             TO   DSP-SDATE.
     MOVE    WK-DATEX             TO   DSP-SDATEX.
*システム時間転送
*    MOVE    WK-TIME(1:6)         TO   DSP-STIME.
     MOVE    WK-TIMEX             TO   DSP-STIMEX.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             項目戻し初期表示                                 *
****************************************************************
 INIT-DSP-SEC2         SECTION.
     MOVE     "INIT-DSP-SEC2"      TO   S-NAME.
     MOVE    ZERO                 TO   TEN-FLG.
     MOVE    ZERO                 TO   DENN-FLG.
     MOVE    DSP-SKBN             TO   WK-SKBN.
     MOVE    DSP-DKBN             TO   WK-DKBN.
     MOVE    DSP-SKBF             TO   WK-SKBF.
     MOVE    DSP-SKBT             TO   WK-SKBT.
     MOVE    DSP-DENKF            TO   WK-DENKF.
     MOVE    DSP-DENKT            TO   WK-DENKT.
     IF      DSP-DENNF  IS  NUMERIC
         MOVE    DSP-TENF             TO   WK-TENF
         MOVE    DSP-TENT             TO   WK-TENT
         MOVE    1                    TO   TEN-FLG
     END-IF.
     IF      DSP-DENNF  IS  NUMERIC
         MOVE    DSP-DENNF            TO   WK-DENNF
         MOVE    DSP-DENNT            TO   WK-DENNT
         MOVE    1                    TO   DENN-FLG
     END-IF.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSK00701.
*システム日付転送
*    MOVE    SYS-DATE             TO   DSP-SDATE.
     MOVE    WK-DATEX             TO   DSP-SDATEX.
*システム時間転送
*    MOVE    WK-TIME(1:6)         TO   DSP-STIME.
     MOVE    WK-TIMEX             TO   DSP-STIMEX.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
     MOVE    WK-SKBN             TO   DSP-SKBN.
     MOVE    WK-DKBN             TO   DSP-DKBN.
     MOVE    WK-SKBF             TO   DSP-SKBF.
     MOVE    WK-SKBT             TO   DSP-SKBT.
     MOVE    WK-DENKF            TO   DSP-DENKF.
     MOVE    WK-DENKT            TO   DSP-DENKT.
     IF      TEN-FLG = 1
         MOVE    WK-TENF             TO   DSP-TENF
         MOVE    WK-TENT             TO   DSP-TENT
     END-IF.
     IF     DENN-FLG = 1
         MOVE    WK-DENNF            TO   DSP-DENNF
         MOVE    WK-DENNT            TO   DSP-DENNT
     END-IF.
*
 INT-DSP-EXIT2.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN.
***  日付区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DKBN.
***  発注日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HFROM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HFROM.
***  発注日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HTO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HTO.
***  納品日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NFROM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NFROM.
***  納品日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NTO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NTO.
***  店舗開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENF.
***  店舗終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENT.
***  伝票番号開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENNF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENNF.
***  伝票番号終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENNT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENNT.
***  出荷場所開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBF.
***  出荷場所終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBT.
***  伝区開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENKF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENKF.
***  伝区終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENKT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENKT.
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3785I   END PROGRAM  >>******************

```
