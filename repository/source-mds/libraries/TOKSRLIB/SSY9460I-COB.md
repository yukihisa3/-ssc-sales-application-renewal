# SSY9460I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9460I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　          *
*    サブシステム　　　　：　流通ＢＭＳ　　　　　　　　　　　　*
*    モジュール名　　　　：　ヨドバシ納期変更出荷確定　　　　　*
*    　　　　　　　　　　　　確認データＣＳＶ出力指示　　　　　*
*    作成日／作成者　　　：　2022/02/15 INOUE                  *
*    処理概要　　　　　　：　ＣＳＶ出力条件指定　　　　　　　　*
*    　　　　　　　　　　：　　　　　　                        *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　                    *
*    　　　　　　　　　　：　　　　　　                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY9460I.
*                 流用:SSY9440I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2022/02/15.
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
*売上伝票ファイル
*    SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
*                      ORGANIZATION        INDEXED
*                      ACCESS    MODE      SEQUENTIAL
*                      RECORD    KEY       DEN-F46
*                                          DEN-F47
*                                          DEN-F01
*                                          DEN-F48
*                                          DEN-F02
*                                          DEN-F04
*                                          DEN-F051
*                                          DEN-F07
*                                          DEN-F112
*                                          DEN-F03
*                      FILE      STATUS    DEN-ST.
*取引先マスタ
*    SELECT  HTOKMS    ASSIGN    TO        TOKMS2
*                      ORGANIZATION        INDEXED
*                      ACCESS    MODE      RANDOM
*                      RECORD    KEY       TOK-F01
*                      FILE      STATUS    TOK-ST.
*倉庫マスタ
*    SELECT  ZSOKMS1   ASSIGN    TO        ZSOKMS1
*                      ORGANIZATION        INDEXED
*                      ACCESS    MODE      RANDOM
*                      RECORD    KEY       SOK-F01
*                      FILE      STATUS    SOK-ST.
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
*    FILE = 売上伝票ファイル                     *
****************************************************************
*FD  SHTDENF
*                      LABEL     RECORD    IS   STANDARD.
*                      COPY      SHTDENF   OF   XFDLIB
*                      JOINING   DEN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
*FD  HTOKMS
*                      BLOCK     CONTAINS  8    RECORDS
*                      LABEL     RECORD    IS   STANDARD.
*                      COPY      HTOKMS    OF   XFDLIB
*                      JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
*FD  ZSOKMS1
*                      BLOCK     CONTAINS  8    RECORDS
*                      LABEL     RECORD    IS   STANDARD.
*                      COPY      ZSOKMS1   OF   XFDLIB
*                      JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY94601  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY9460I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY94601".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  ZSOKMS1-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WK-DEN.
     03  WK-DENNO                 PIC  9(09)  OCCURS 20.
 01  IX                           PIC  9(02)  VALUE  ZERO.
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
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（日付）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"バッチ_（時間）論理エラー".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"納品日を入力してください".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"発行区分エラーです".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"実行日付　論理エラー".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  10  PIC   N(20).
*
 01  FILE-ERR.
     03  DEN-ERR           PIC N(20) VALUE
                        NC"売上伝票ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-DATE1          PIC  9(08).
 01  LINK-DATE2          PIC  9(08).
 01  LINK-HKKBN          PIC  X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-DATE1
                                     LINK-DATE2
                                     LINK-HKKBN.
**************************************************************
 DECLARATIVES.
*DEN-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
*    DISPLAY     DEN-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     DEN-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
*TOK-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
*    DISPLAY     TOK-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     TOK-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
*SOK-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE ZSOKMS1.
*    DISPLAY     SOK-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     SOK-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
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
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
*    OPEN     INPUT     SHTDENF   HTOKMS   ZSOKMS1.
*ワークの初期化
     INITIALIZE         FLG-AREA.
     INITIALIZE         LINK-DATE1
                        LINK-DATE2.
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
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
         WHEN      "2"  PERFORM   DSP-PARA-SEC
         WHEN      "3"  PERFORM   DSP-PARA-SEC
*        WHEN      "4"  PERFORM   DSP-PARA-SEC
*        WHEN      "5"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "4"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 - 5 )            2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                IF       PSW     =    "2"
                         MOVE   "1"   TO   PSW
                END-IF
                IF       PSW     =    "3"
                         MOVE   "2"   TO   PSW
                END-IF
                IF       PSW     =    "4"
                         MOVE   "3"   TO   PSW
                END-IF
*               IF       PSW     =    "5"
*                        MOVE   "4"   TO   PSW
*               END-IF
*               IF       PSW     =    "6"
*                        MOVE   "5"   TO   PSW
*               END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
     IF       PSW   =  "1"
              GO             TO   PARA-CHK-01
     END-IF.
     IF       PSW   =  "2"
              GO             TO   PARA-CHK-02
     END-IF.
     IF       PSW   =  "3"
              GO             TO   PARA-CHK-03
     END-IF.
*    IF       PSW   =  "4"
*             GO             TO   PARA-CHK-04
*    END-IF.
*    IF       PSW   =  "5"
*             GO             TO   PARA-CHK-05
*    END-IF.
*
*PARA-CHK-01.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
*    IF       DSP-BTYMD  NOT NUMERIC
*        OR   DSP-BTYMD  =  ZERO
*             MOVE   1       TO   ERR-FLG
*             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTYMD
*             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTYMD
*             GO             TO   PARA-CHK-EXIT
*    ELSE
***           バッチ_（日付）論理チェック
*             MOVE     "2"            TO   LINK-IN-KBN
*             MOVE     ZERO           TO   LINK-IN-YMD6
*             MOVE     DSP-BTYMD      TO   LINK-IN-YMD8
*             MOVE     ZERO           TO   LINK-OUT-RET
*             MOVE     ZERO           TO   LINK-OUT-YMD
*             CALL     "SKYDTCKB"     USING   LINK-IN-KBN
*                                             LINK-IN-YMD6
*                                             LINK-IN-YMD8
*                                             LINK-OUT-RET
*                                             LINK-OUT-YMD
*             IF   LINK-OUT-RET   = 9
*                  MOVE  4        TO   ERR-FLG
*                  MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTYMD
*                  MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTYMD
*                  GO             TO   PARA-CHK-EXIT
*             END-IF
*
*             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTYMD
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTYMD
*    END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
*    IF       DSP-BTTIME  NOT NUMERIC
*        OR   DSP-BTTIME  =   ZERO
*                  MOVE   1       TO   ERR-FLG
*                  MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
*                  MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTIME
*                  GO             TO   PARA-CHK-EXIT
*    ELSE
***           バッチ_（時間）論理チェック
*             MOVE     DSP-BTTIME    TO  WK-JIKAN
*             IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
*                      WK-MM  <   ZERO  OR  WK-MM  >  59
*                      IF     ERR-FLG  =  ZERO
*                             MOVE    5    TO    ERR-FLG
*                      END-IF
*                      MOVE "R"    TO EDIT-OPTION OF DSP-BTTIME
*                      MOVE "C"    TO EDIT-CURSOR OF DSP-BTTIME
*                      GO          TO PARA-CHK-EXIT
*             ELSE
*                      MOVE "M"    TO EDIT-OPTION OF DSP-BTTIME
*                      MOVE SPACE  TO EDIT-CURSOR OF DSP-BTTIME
*             END-IF
*    END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
*    IF       DSP-BTTOKC  NOT NUMERIC
*        OR   DSP-BTTOKC  =  ZERO
*             MOVE   1       TO   ERR-FLG
*             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTOKC
*             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTOKC
*             GO             TO   PARA-CHK-EXIT
*    ELSE
***           取引先ＲＥＡＤ
*             MOVE     DSP-BTTOKC TO   TOK-F01
*             READ     HTOKMS
*             INVALID
*                    MOVE   2     TO   ERR-FLG
*                    MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTOKC
*                    MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTOKC
*                    MOVE   SPACE TO   DSP-BTTOKN
*                    GO           TO   PARA-CHK-EXIT
*             NOT INVALID
*                    MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTOKC
*                    MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTOKC
*                    MOVE  TOK-F03  TO DSP-BTTOKN
*             END-READ
*    END-IF.
*
*売上伝票ファイル存在チェック
*    MOVE     SPACE          TO   DEN-REC.
*    INITIALIZE                   DEN-REC.
*    MOVE     DSP-BTYMD      TO   DEN-F46.
*    MOVE     DSP-BTTIME     TO   DEN-F47.
*    MOVE     DSP-BTTOKC     TO   DEN-F01.
*    MOVE     SPACE          TO   DEN-F48.
*    START    SHTDENF   KEY  IS   >=   DEN-F46  DEN-F47  DEN-F01
*                                      DEN-F48  DEN-F02  DEN-F04
*                                      DEN-F051 DEN-F07  DEN-F112
*                                      DEN-F03
*    INVALID
*             MOVE      9    TO   ERR-FLG
*             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTYMD
*             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTIME
*             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTOKC
*             MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTYMD
*             GO             TO   PARA-CHK-EXIT
*    NOT INVALID
*             売上ファイル存在チェック
*             READ     SHTDENF
*             AT END
*                  MOVE   9   TO   ERR-FLG
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-BTYMD
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTOKC
*                  MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTYMD
*                  GO         TO   PARA-CHK-EXIT
*             END-READ
*             IF   DSP-BTYMD   =   DEN-F46 AND
*                  DSP-BTTIME  =   DEN-F47 AND
*                  DSP-BTTOKC  =   DEN-F01
*                  MOVE  SPACE TO  EDIT-CURSOR OF DSP-BTYMD
*             ELSE
*                  MOVE   9   TO   ERR-FLG
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-BTYMD
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTOKC
*                  MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTYMD
*                  GO         TO   PARA-CHK-EXIT
*             END-IF
*    END-START.
*
*TEST
*    DISPLAY "ERR-FLG=" ERR-FLG UPON CONS.
*TEST
*    IF       ERR-FLG  =  0
*             MOVE    "2"     TO   PSW
*    END-IF.
*    GO                TO    PARA-CHK-99.
*
*PARA-CHK-02.
*倉庫コードチェック
*    IF       DSP-SOKCD  NOT = SPACE
*             MOVE       DSP-SOKCD  TO  SOK-F01
*             PERFORM    ZSOKMS1-READ-SEC
*             IF         ZSOKMS1-INV-FLG = "INV"
*                  IF    ERR-FLG     = ZERO
*                        MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
*                        MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
*                        MOVE   7   TO  ERR-FLG
*                  ELSE
*                        MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
*                  END-IF
*             ELSE
*                        MOVE  SOK-F02              TO DSP-SOKNM
*                        MOVE  "M"  TO  EDIT-OPTION OF DSP-SOKCD
*                        MOVE  " "  TO  EDIT-CURSOR OF DSP-SOKCD
*             END-IF
*    ELSE
*             IF    ERR-FLG     = ZERO
*                   MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
*                   MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
*                   MOVE   7   TO  ERR-FLG
*             ELSE
*                   MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
*             END-IF
*    END-IF.
*
*    IF       ERR-FLG  =  0
*             MOVE    "3"     TO   PSW
*    END-IF.
*    GO                TO    PARA-CHK-99.
*
*PARA-CHK-03.
 PARA-CHK-01.
*実行日開始チェック
*    未入力チェック
     IF       DSP-JSDATE NOT NUMERIC
              MOVE   ZERO    TO   DSP-JSDATE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JSDATE
              MOVE  " "      TO   EDIT-CURSOR  OF  DSP-JSDATE
     ELSE
*             論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JSDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JSDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JSDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JSDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JSDATE
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "2"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
*
 PARA-CHK-02.
*実行日終了チェック
*    未入力チェック
     IF       DSP-JEDATE NOT NUMERIC
              MOVE   99999999  TO   DSP-JEDATE
              MOVE  "M"        TO   EDIT-OPTION  OF  DSP-JEDATE
              MOVE  " "        TO   EDIT-CURSOR  OF  DSP-JEDATE
     ELSE
*             論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JEDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JEDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JEDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JEDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JEDATE
     END-IF.
*
*実行日大小チェック
     IF       DSP-JSDATE >   DSP-JEDATE
         IF   ERR-FLG      =  0
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JSDATE
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JEDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JSDATE
              GO             TO   PARA-CHK-EXIT
         ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JSDATE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JEDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JSDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JEDATE
         END-IF
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "3"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
*PARA-CHK-04.
 PARA-CHK-03.
*発行区分チェック
*    規定値チェック
     IF     ( DSP-HKKBN  NOT = " " )
        AND ( DSP-HKKBN  NOT = "1" )
        AND ( DSP-HKKBN  NOT = "2" )
              MOVE   9       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HKKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HKKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HKKBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HKKBN
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "4"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
*
 PARA-CHK-99.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             倉庫マスタ索引                                   *
****************************************************************
*ZSOKMS1-READ-SEC     SECTION.
*    MOVE     "ZSOKMS1-READ-SEC" TO   S-NAME.
*
*    READ      ZSOKMS1
*       INVALID
*              MOVE    "INV"     TO   ZSOKMS1-INV-FLG
*       NOT INVALID
*              MOVE    "   "     TO   ZSOKMS1-INV-FLG
*    END-READ.
*
*ZSOKMS1-READ-EXIT.
*    EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
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
                MOVE    DSP-JSDATE    TO   LINK-DATE1
                MOVE    DSP-JEDATE    TO   LINK-DATE2
                MOVE    DSP-HKKBN     TO   LINK-HKKBN
                MOVE    "END"              TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM          INIT-DSP-SEC
*
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
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
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
*        WHEN   "4"
*               MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
*        WHEN   "5"
*               MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      確認
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY94601"          TO   DSP-FMT.
     WRITE    DSP-FSY94601.
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
*　　　実行日付開始
         WHEN   "1"
*               MOVE    "GRP001"  TO   DSP-GRP
                MOVE    "JSDATE"  TO   DSP-GRP
*　　　実行日付終了
         WHEN   "2"
                MOVE    "JEDATE"  TO   DSP-GRP
*　　　発行区分　
         WHEN   "3"
                MOVE    "HKKBN"   TO   DSP-GRP
*　　　確認
         WHEN   "4"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY94601"           TO   DSP-FMT.
     READ     DSPFILE.
*
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
     MOVE    SPACE                TO   DSP-FSY94601.
*プログラムＩＤ転送
     MOVE    WORK-PGID            TO   DSP-PGID.
*ＦＯＲＭＩＤ転送
     MOVE    WORK-FORMID          TO   DSP-FORMID.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*項目属性クリア　
     PERFORM DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  実行日付開始　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JSDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JSDATE.
***  実行日付終了　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JEDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JEDATE.
***  発行区分　　　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HKKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
*    CLOSE             SHTDENF  HTOKMS  ZSOKMS1  DSPFILE.
     CLOSE             DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY9460I   END PROGRAM  >>******************

```
