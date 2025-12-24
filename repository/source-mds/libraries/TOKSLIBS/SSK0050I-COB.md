# SSK0050I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0050I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス　　　            *
*    モジュール名　　　　：　返品計上確認リスト発行指示        *
*    作成日／更新日　　　：　2014/03/06                        *
*    作成者／更新者　　　：　NAV/MIURA                         *
*    処理概要　　　　　　：　リスト＆ＣＳＶ出力の指示を画面より*
*                        ：　行なう　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSK0050I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         14/03/19.
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
                       COPY      FSK00501  OF   XMDLIB
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
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
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
     03  WK-KEIKBN                PIC   X(01)  VALUE  SPACE.
     03  WK-TANF                  PIC   X(02)  VALUE  SPACE.
     03  WK-TANT                  PIC   X(02)  VALUE  SPACE.
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
             VALUE NC"正しい計上区分を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい担当者範囲を入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付区分を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付を入力して下さい。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付範囲を入力して下さい。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　".
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
 01  LINK-TANTO              PIC  X(02).
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
                                         LINK-TANTO
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
*計上区分入力
         WHEN      "2"  PERFORM   DSP-PAR2-SEC
*担当者区分
         WHEN      "3"  PERFORM   DSP-PAR3-SEC
*日付区分入力
         WHEN      "4"  PERFORM   DSP-PAR4-SEC
*日付入力
         WHEN      "A"  PERFORM   DSP-PAR41-SEC
         WHEN      "N"  PERFORM   DSP-PAR41-SEC
         WHEN      "K"  PERFORM   DSP-PAR41-SEC
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
                MOVE    "2"      TO   PSW
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
                EVALUATE   DSP-DKBN
                  WHEN   "1"
                    MOVE  "A"  TO    PSW
                  WHEN   "2"
                    MOVE  "N"  TO    PSW
                  WHEN   "3"
                    MOVE  "K"  TO    PSW
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
                MOVE    "3"      TO   PSW
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR4-SEC
     END-EVALUATE.
*
 DSP-PAR4-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 41)                2.1       *
****************************************************************
 DSP-PAR41-SEC         SECTION.
     MOVE     "DSP-PAR41-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK41-SEC
*               MOVE    "5"      TO   PSW
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
                MOVE    "4"      TO   PSW
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-PAR41-SEC
     END-EVALUATE.
*
 DSP-PAR41-EXIT.
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
*             パラメタチェック2                      2.1.1     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*計上区分チェック
     IF       DSP-KEIKBN   =  "1"  OR  "2"  OR  "3"  OR  "4"
              CONTINUE
     ELSE
              MOVE   2       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KEIKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KEIKBN
              GO             TO   PARA-CHK2-EXIT
     END-IF.
     IF       DSP-KEIKBN   =  "1"
         PERFORM   INIT-DSP-SEC3
         MOVE    "5"       TO    PSW
     ELSE
         MOVE    "3"       TO    PSW
     END-IF.
*
 PARA-CHK2-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック3                      2.1.1     *
****************************************************************
 PARA-CHK3-SEC             SECTION.
     MOVE     "PARA-CHK3-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*大小チェック
     IF  DSP-TANF >  DSP-TANT
         MOVE      3    TO   ERR-FLG
         MOVE     "R"   TO   EDIT-OPTION  OF   DSP-TANF
         MOVE     "R"   TO   EDIT-OPTION  OF   DSP-TANT
         MOVE     "C"   TO   EDIT-CURSOR  OF   DSP-TANF
         GO    TO     PARA-CHK3-EXIT
     END-IF.
     MOVE     "4"   TO    PSW.
*
 PARA-CHK3-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック4                      2.1.2     *
****************************************************************
 PARA-CHK4-SEC             SECTION.
     MOVE     "PARA-CHK4-SEC"     TO   S-NAME.
*属性を初期化する
*    PERFORM  DSP-SYOKI-SEC.
     PERFORM  INIT-DSP-SEC2.
*日付区分チェック
     IF       DSP-DKBN   =  "1"  OR  "2"  OR  "3"
              CONTINUE
     ELSE
              MOVE   4       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DKBN
              GO             TO   PARA-CHK4-EXIT
     END-IF.
*
 PARA-CHK4-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック41                      2.1.3
****************************************************************
 PARA-CHK41-SEC             SECTION.
     MOVE     "PARA-CHK41-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
     EVALUATE   DSP-DKBN
       WHEN   "1"
*数値初期値セット
         IF       DSP-AFROM  =  ZERO
         OR       DSP-AFROM  NOT  NUMERIC
                 MOVE  ZERO     TO   DSP-AFROM
         ELSE
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-AFROM  TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  5         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-AFROM
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-AFROM
                   GO   TO   PARA-CHK41-EXIT
           END-IF
         END-IF
         IF       DSP-ATO  =  ZERO
         OR       DSP-ATO  NOT  NUMERIC
                   MOVE  999999999   TO   DSP-ATO
         ELSE
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-ATO    TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  5         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-ATO
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-ATO
                GO   TO   PARA-CHK41-EXIT
           END-IF
         END-IF
*日付大小チェック
         IF       DSP-AFROM  >  DSP-ATO
              MOVE      6    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-AFROM
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-ATO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-AFROM
              GO             TO   PARA-CHK41-EXIT
         END-IF
       WHEN   "2"
         IF       DSP-NFROM  =  ZERO
         OR       DSP-NFROM  NOT  NUMERIC
                 MOVE  ZERO     TO   DSP-NFROM
         ELSE
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-NFROM  TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  5         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-NFROM
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-NFROM
                   GO   TO   PARA-CHK41-EXIT
           END-IF
         END-IF
         IF       DSP-NTO  =  ZERO
         OR       DSP-NTO  NOT  NUMERIC
                   MOVE  999999999   TO   DSP-NTO
         ELSE
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-NTO    TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  5         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-NTO
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-NTO
                GO   TO   PARA-CHK41-EXIT
           END-IF
         END-IF
*日付大小チェック
         IF       DSP-NFROM  >  DSP-NTO
              MOVE      6    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NFROM
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-NTO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-NFROM
              GO             TO   PARA-CHK41-EXIT
         END-IF
       WHEN   "3"
         IF       DSP-KFROM  =  ZERO
         OR       DSP-KFROM  NOT  NUMERIC
                 MOVE  ZERO     TO   DSP-KFROM
         ELSE
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-KFROM  TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  5         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-KFROM
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-KFROM
                   GO   TO   PARA-CHK41-EXIT
           END-IF
         END-IF
         IF       DSP-KTO  =  ZERO
         OR       DSP-KTO  NOT  NUMERIC
                   MOVE  999999999   TO   DSP-KTO
         ELSE
           MOVE     "2"        TO        LINK-IN-KBN
           MOVE     DSP-KTO    TO        LINK-IN-YMD8
           CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
           IF   LINK-OUT-RET   NOT =     ZERO
                   MOVE  5         TO   ERR-FLG
                   MOVE  "C"       TO   EDIT-CURSOR OF DSP-KTO
                   MOVE  "R"       TO   EDIT-OPTION OF DSP-KTO
                GO   TO   PARA-CHK41-EXIT
           END-IF
         END-IF
*日付大小チェック
         IF       DSP-KFROM  >  DSP-KTO
              MOVE      6    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KFROM
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KTO
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KFROM
              GO             TO   PARA-CHK41-EXIT
         END-IF
     END-EVALUATE.
     MOVE    "5"       TO    PSW.
*
 PARA-CHK41-EXIT.
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
                EVALUATE   DSP-DKBN
                  WHEN   "1"
                    MOVE    "2"           TO   LINK-DKBN
                    MOVE    DSP-AFROM     TO   LINK-DFROM
                    MOVE    DSP-ATO       TO   LINK-DTO
                  WHEN   "2"
                    MOVE    "6"           TO   LINK-DKBN
                    MOVE    DSP-NFROM     TO   LINK-DFROM
                    MOVE    DSP-NTO       TO   LINK-DTO
                  WHEN   "3"
                    MOVE    "7"           TO   LINK-DKBN
                    MOVE    DSP-KFROM     TO   LINK-DFROM
                    MOVE    DSP-KTO       TO   LINK-DTO
                END-EVALUATE
                MOVE    DSP-KEIKBN    TO   LINK-KKBN
                MOVE    DSP-TANF      TO   LINK-TANFROM
                MOVE    DSP-TANT      TO   LINK-TANTO
                MOVE    "END"         TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
             IF DSP-KEIKBN = "1"
                 MOVE  "2"  TO    PSW
             ELSE
                EVALUATE   DSP-DKBN
                  WHEN   "1"
                    MOVE  "A"  TO    PSW
                  WHEN   "2"
                    MOVE  "N"  TO    PSW
                  WHEN   "3"
                    MOVE  "K"  TO    PSW
                END-EVALUATE
             END-IF
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
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "A"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "N"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "K"
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
     MOVE    "SSK0050I"          TO   DSP-PGID.
     MOVE    "FSK00501"          TO   DSP-FORMID.
     MOVE    "FSK00501"          TO   DSP-FMT.
     WRITE    DSP-FSK00501.
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
         WHEN   "3"
                MOVE    "PARGR3"  TO   DSP-GRP
*パラメタ項目
         WHEN   "4"
                MOVE    "PARGR4"  TO   DSP-GRP
*パラメタ項目
         WHEN   "A"
                MOVE    "PARAGA"  TO   DSP-GRP
         WHEN   "N"
                MOVE    "PARAGN"  TO   DSP-GRP
         WHEN   "K"
                MOVE    "PARAGK"  TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "SSK0050I"           TO   DSP-PGID.
     MOVE    "FSK00501"           TO   DSP-FORMID.
     MOVE    "FSK00501"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSK00501.
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
     MOVE    DSP-SKBN             TO   WK-SKBN.
     MOVE    DSP-DKBN             TO   WK-DKBN.
     MOVE    DSP-KEIKBN             TO   WK-KEIKBN.
     MOVE    DSP-TANF            TO   WK-TANF.
     MOVE    DSP-TANT            TO   WK-TANT.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSK00501.
*システム日付転送
*    MOVE    SYS-DATE             TO   DSP-SDATE.
     MOVE    WK-DATEX             TO   DSP-SDATEX.
*システム時間転送
*    MOVE    WK-TIME(1:6)         TO   DSP-STIME.
     MOVE    WK-DATEX             TO   DSP-SDATEX.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
     MOVE    WK-SKBN             TO   DSP-SKBN.
     MOVE    WK-DKBN             TO   DSP-DKBN.
     MOVE    WK-KEIKBN             TO   DSP-KEIKBN.
     MOVE    WK-TANF             TO   DSP-TANF.
     MOVE    WK-TANT             TO   DSP-TANT.
*
 INT-DSP-EXIT2.
     EXIT.
****************************************************************
*             項目戻し初期表示                                 *
****************************************************************
 INIT-DSP-SEC3         SECTION.
     MOVE     "INIT-DSP-SEC3"      TO   S-NAME.
     MOVE    DSP-SKBN             TO   WK-SKBN.
     MOVE    DSP-KEIKBN             TO   WK-KEIKBN.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSK00501.
*システム日付転送
*    MOVE    SYS-DATE             TO   DSP-SDATE.
     MOVE    WK-DATEX             TO   DSP-SDATEX.
*システム時間転送
*    MOVE    WK-TIME(1:6)         TO   DSP-STIME.
     MOVE    WK-DATEX             TO   DSP-SDATEX.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
     MOVE    WK-SKBN             TO   DSP-SKBN.
     MOVE    WK-KEIKBN           TO   DSP-KEIKBN.
*
 INT-DSP-EXIT3.
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
***  計上区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KEIKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KEIKBN.
***  担当者
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TANF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TANF.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TANT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TANT.
***  日付区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DKBN.
***  検収日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-AFROM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-AFROM.
***  検収日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-ATO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-ATO.
***  入力日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NFROM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NFROM.
***  入力日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NTO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NTO.
***  計上日開始
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KFROM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KFROM.
***  計上日終了
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KTO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KTO.
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
