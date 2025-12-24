# SSY9410I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9410I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受注　　　　　　　　　　          *
*    サブシステム　　　　：　流通ＢＭＳ　　　　　　　　　　　　*
*    モジュール名　　　　：　ヨドバシ納期一括変更指示　　　　　*
*    作成日／作成者　　　：　2022/02/10 INOUE                  *
*    処理概要　　　　　　：　一括変更条件指定　　　　　　　　　*
*    　　　　　　　　　　：　　　　　　                        *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　                    *
*    　　　　　　　　　　：　　　　　　                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY9410I.
*                 流用:SSY5210I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2022/02/10.
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
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT  ZSOKMS1   ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
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
 FD  HTOKMS
*                      BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS1
*                      BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS1   OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY94101  OF   XMDLIB
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
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY9410I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY94101".
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
             VALUE NC"－－－－－－－－－－－－－－".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"納品日を入力してください".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"売上伝票ファイルに登録されていません".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"納品日　論理エラー".
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
 01  LINK-DATE           PIC  9(08).
 01  LINK-TIME           PIC  9(04).
 01  LINK-TORICD         PIC  X(08).
 01  LINK-SOKCD          PIC  X(02).
 01  LINK-FDATE          PIC  9(08).
 01  LINK-TDATE          PIC  9(08).
 01  LINK-DENNO.
     03  LINK-DENNO-01   PIC  9(09).
     03  LINK-DENNO-02   PIC  9(09).
     03  LINK-DENNO-03   PIC  9(09).
     03  LINK-DENNO-04   PIC  9(09).
     03  LINK-DENNO-05   PIC  9(09).
     03  LINK-DENNO-06   PIC  9(09).
     03  LINK-DENNO-07   PIC  9(09).
     03  LINK-DENNO-08   PIC  9(09).
     03  LINK-DENNO-09   PIC  9(09).
     03  LINK-DENNO-10   PIC  9(09).
     03  LINK-DENNO-11   PIC  9(09).
     03  LINK-DENNO-12   PIC  9(09).
     03  LINK-DENNO-13   PIC  9(09).
     03  LINK-DENNO-14   PIC  9(09).
     03  LINK-DENNO-15   PIC  9(09).
     03  LINK-DENNO-16   PIC  9(09).
     03  LINK-DENNO-17   PIC  9(09).
     03  LINK-DENNO-18   PIC  9(09).
     03  LINK-DENNO-19   PIC  9(09).
     03  LINK-DENNO-20   PIC  9(09).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-DATE
                                     LINK-TIME
                                     LINK-TORICD
                                     LINK-SOKCD
                                     LINK-FDATE
                                     LINK-TDATE
                                     LINK-DENNO.
**************************************************************
 DECLARATIVES.
*DEN-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
*    DISPLAY     DEN-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     DEN-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS1.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
     OPEN     INPUT     HTOKMS   ZSOKMS1.
*ワークの初期化
     INITIALIZE         FLG-AREA.
     INITIALIZE         LINK-DATE
                        LINK-TIME
                        LINK-TORICD
                        LINK-SOKCD
                        LINK-FDATE
                        LINK-TDATE
                        LINK-DENNO-01
                        LINK-DENNO-02
                        LINK-DENNO-03
                        LINK-DENNO-04
                        LINK-DENNO-05
                        LINK-DENNO-06
                        LINK-DENNO-07
                        LINK-DENNO-08
                        LINK-DENNO-09
                        LINK-DENNO-10
                        LINK-DENNO-11
                        LINK-DENNO-12
                        LINK-DENNO-13
                        LINK-DENNO-14
                        LINK-DENNO-15
                        LINK-DENNO-16
                        LINK-DENNO-17
                        LINK-DENNO-18
                        LINK-DENNO-19
                        LINK-DENNO-20.
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
         WHEN      "4"  PERFORM   DSP-PARA-SEC
         WHEN      "5"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "6"  PERFORM   DSP-KAKU-SEC
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
*               MOVE            "1"   TO   PSW
                IF       PSW     =    "2"
                         MOVE   "1"   TO   PSW
                END-IF
                IF       PSW     =    "3"
                         MOVE   "2"   TO   PSW
                END-IF
                IF       PSW     =    "4"
                         MOVE   "3"   TO   PSW
                END-IF
                IF       PSW     =    "5"
                         MOVE   "4"   TO   PSW
                END-IF
                IF       PSW     =    "6"
                         MOVE   "5"   TO   PSW
                END-IF
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
     IF       PSW   =  "4"
              GO             TO   PARA-CHK-04
     END-IF.
     IF       PSW   =  "5"
              GO             TO   PARA-CHK-05
     END-IF.
*
 PARA-CHK-01.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTYMD  NOT NUMERIC
         OR   DSP-BTYMD  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTYMD
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTYMD
              GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-BTYMD      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTYMD
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTYMD
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTYMD
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTYMD
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       DSP-BTTIME  NOT NUMERIC
         OR   DSP-BTTIME  =   ZERO
                   MOVE   1       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTIME
                   GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-BTTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    5    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-BTTIME
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-BTTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-BTTIME
              END-IF
     END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       DSP-BTTOKC  NOT NUMERIC
         OR   DSP-BTTOKC  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTOKC
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTOKC
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTOKC TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTOKC
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTOKC
                     MOVE   SPACE TO   DSP-BTTOKN
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTOKC
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTOKC
                     MOVE  TOK-F03  TO DSP-BTTOKN
              END-READ
     END-IF.
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
     IF       ERR-FLG  =  0
              MOVE    "2"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
 PARA-CHK-02.
*倉庫コードチェック
     IF       DSP-SOKCD  NOT = SPACE
              MOVE       DSP-SOKCD  TO  SOK-F01
              PERFORM    ZSOKMS1-READ-SEC
              IF         ZSOKMS1-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
                   END-IF
              ELSE
                         MOVE  SOK-F02              TO DSP-SOKNM
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SOKCD
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SOKCD
              END-IF
     ELSE
*             MOVE  "M"  TO  EDIT-OPTION OF DSP-SOKCD
*             MOVE  " "  TO  EDIT-CURSOR OF DSP-SOKCD
              IF    ERR-FLG     = ZERO
                    MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
                    MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKCD
                    MOVE   7   TO  ERR-FLG
              ELSE
                    MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKCD
              END-IF
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "3"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
 PARA-CHK-03.
*変更前納品日チェック
*    未入力チェック
     IF       DSP-FDATE  NOT NUMERIC
         OR   DSP-FDATE  =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-FDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-FDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
*             論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-FDATE      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-FDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-FDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-FDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-FDATE
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "4"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
 PARA-CHK-04.
*変更後納品日チェック
*    未入力チェック
     IF       DSP-TDATE  NOT NUMERIC
         OR   DSP-TDATE  =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
*             論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-TDATE      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-TDATE
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "5"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
 PARA-CHK-05.
*伝票番号チェック
     MOVE     ZERO                TO   ERR-FLG.
*1
*    IF     ( DSP-DEN001 NOT NUMERIC ) OR
*           ( DSP-DEN001 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN001
*    END-IF.
*2
*    IF     ( DSP-DEN002 NOT NUMERIC ) OR
*           ( DSP-DEN002 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN002
*    END-IF.
*3
*    IF     ( DSP-DEN003 NOT NUMERIC ) OR
*           ( DSP-DEN003 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN003
*    END-IF.
*4
*    IF     ( DSP-DEN004 NOT NUMERIC ) OR
*           ( DSP-DEN004 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN004
*    END-IF.
*5
*    IF     ( DSP-DEN005 NOT NUMERIC ) OR
*           ( DSP-DEN005 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN005
*    END-IF.
*6
*    IF     ( DSP-DEN006 NOT NUMERIC ) OR
*           ( DSP-DEN006 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN006
*    END-IF.
*7
*    IF     ( DSP-DEN007 NOT NUMERIC ) OR
*           ( DSP-DEN007 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN007
*    END-IF.
*8
*    IF     ( DSP-DEN008 NOT NUMERIC ) OR
*           ( DSP-DEN008 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN008
*    END-IF.
*9
*    IF     ( DSP-DEN009 NOT NUMERIC ) OR
*           ( DSP-DEN009 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN009
*    END-IF.
*10
*    IF     ( DSP-DEN010 NOT NUMERIC ) OR
*           ( DSP-DEN010 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN010
*    END-IF.
*11
*    IF     ( DSP-DEN011 NOT NUMERIC ) OR
*           ( DSP-DEN011 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN011
*    END-IF.
*12
*    IF     ( DSP-DEN012 NOT NUMERIC ) OR
*           ( DSP-DEN012 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN012
*    END-IF.
*13
*    IF     ( DSP-DEN013 NOT NUMERIC ) OR
*           ( DSP-DEN013 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN013
*    END-IF.
*14
*    IF     ( DSP-DEN014 NOT NUMERIC ) OR
*           ( DSP-DEN014 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN014
*    END-IF.
*15
*    IF     ( DSP-DEN015 NOT NUMERIC ) OR
*           ( DSP-DEN015 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN015
*    END-IF.
*16
*    IF     ( DSP-DEN016 NOT NUMERIC ) OR
*           ( DSP-DEN016 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN016
*    END-IF.
*17
*    IF     ( DSP-DEN017 NOT NUMERIC ) OR
*           ( DSP-DEN017 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN017
*    END-IF.
*18
*    IF     ( DSP-DEN018 NOT NUMERIC ) OR
*           ( DSP-DEN018 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN018
*    END-IF.
*19
*    IF     ( DSP-DEN019 NOT NUMERIC ) OR
*           ( DSP-DEN019 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN019
*    END-IF.
*20
*    IF     ( DSP-DEN020 NOT NUMERIC ) OR
*           ( DSP-DEN020 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN020
*    END-IF.
*
     IF       ERR-FLG     = ZERO
              MOVE    "6"       TO    PSW
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
 ZSOKMS1-READ-SEC     SECTION.
     MOVE     "ZSOKMS1-READ-SEC" TO   S-NAME.
*
     READ      ZSOKMS1
        INVALID
               MOVE    "INV"     TO   ZSOKMS1-INV-FLG
        NOT INVALID
               MOVE    "   "     TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
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
                MOVE    DSP-BTYMD     TO   LINK-DATE
                MOVE    DSP-BTTIME    TO   LINK-TIME
                MOVE    DSP-BTTOKC    TO   LINK-TORICD
                MOVE    DSP-SOKCD     TO   LINK-SOKCD
                MOVE    DSP-FDATE     TO   LINK-FDATE
                MOVE    DSP-TDATE     TO   LINK-TDATE
                MOVE    ALL "0"       TO   WK-DEN
                MOVE    1             TO   IX
                IF    ( DSP-DEN001         NUMERIC ) AND
                      ( DSP-DEN001 NOT =   ZERO    )
                        MOVE  DSP-DEN001   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                END-IF
                IF    ( DSP-DEN002         NUMERIC ) AND
                      ( DSP-DEN002 NOT =   ZERO    )
                    IF  DSP-DEN002 NOT =   DSP-DEN001
                        MOVE  DSP-DEN002   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN003         NUMERIC ) AND
                      ( DSP-DEN003 NOT =   ZERO    )
                    IF  DSP-DEN003 NOT =   DSP-DEN001 AND
                        DSP-DEN003 NOT =   DSP-DEN002
                        MOVE  DSP-DEN003   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN004         NUMERIC ) AND
                      ( DSP-DEN004 NOT =   ZERO    )
                    IF  DSP-DEN004 NOT =   DSP-DEN001 AND
                        DSP-DEN004 NOT =   DSP-DEN002 AND
                        DSP-DEN004 NOT =   DSP-DEN003
                        MOVE  DSP-DEN004   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN005         NUMERIC ) AND
                      ( DSP-DEN005 NOT =   ZERO    )
                    IF  DSP-DEN005 NOT =   DSP-DEN001 AND
                        DSP-DEN005 NOT =   DSP-DEN002 AND
                        DSP-DEN005 NOT =   DSP-DEN003 AND
                        DSP-DEN005 NOT =   DSP-DEN004
                        MOVE  DSP-DEN005   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN006         NUMERIC ) AND
                      ( DSP-DEN006 NOT =   ZERO    )
                    IF  DSP-DEN006 NOT =   DSP-DEN001 AND
                        DSP-DEN006 NOT =   DSP-DEN002 AND
                        DSP-DEN006 NOT =   DSP-DEN003 AND
                        DSP-DEN006 NOT =   DSP-DEN004 AND
                        DSP-DEN006 NOT =   DSP-DEN005
                        MOVE  DSP-DEN006   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN007         NUMERIC ) AND
                      ( DSP-DEN007 NOT =   ZERO    )
                    IF  DSP-DEN007 NOT =   DSP-DEN001 AND
                        DSP-DEN007 NOT =   DSP-DEN002 AND
                        DSP-DEN007 NOT =   DSP-DEN003 AND
                        DSP-DEN007 NOT =   DSP-DEN004 AND
                        DSP-DEN007 NOT =   DSP-DEN005 AND
                        DSP-DEN007 NOT =   DSP-DEN006
                        MOVE  DSP-DEN007   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN008         NUMERIC ) AND
                      ( DSP-DEN008 NOT =   ZERO    )
                    IF  DSP-DEN008 NOT =   DSP-DEN001 AND
                        DSP-DEN008 NOT =   DSP-DEN002 AND
                        DSP-DEN008 NOT =   DSP-DEN003 AND
                        DSP-DEN008 NOT =   DSP-DEN004 AND
                        DSP-DEN008 NOT =   DSP-DEN005 AND
                        DSP-DEN008 NOT =   DSP-DEN006 AND
                        DSP-DEN008 NOT =   DSP-DEN007
                        MOVE  DSP-DEN008   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN009         NUMERIC ) AND
                      ( DSP-DEN009 NOT =   ZERO    )
                    IF  DSP-DEN009 NOT =   DSP-DEN001 AND
                        DSP-DEN009 NOT =   DSP-DEN002 AND
                        DSP-DEN009 NOT =   DSP-DEN003 AND
                        DSP-DEN009 NOT =   DSP-DEN004 AND
                        DSP-DEN009 NOT =   DSP-DEN005 AND
                        DSP-DEN009 NOT =   DSP-DEN006 AND
                        DSP-DEN009 NOT =   DSP-DEN007 AND
                        DSP-DEN009 NOT =   DSP-DEN008
                        MOVE  DSP-DEN009   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN010         NUMERIC ) AND
                      ( DSP-DEN010 NOT =   ZERO    )
                    IF  DSP-DEN010 NOT =   DSP-DEN001 AND
                        DSP-DEN010 NOT =   DSP-DEN002 AND
                        DSP-DEN010 NOT =   DSP-DEN003 AND
                        DSP-DEN010 NOT =   DSP-DEN004 AND
                        DSP-DEN010 NOT =   DSP-DEN005 AND
                        DSP-DEN010 NOT =   DSP-DEN006 AND
                        DSP-DEN010 NOT =   DSP-DEN007 AND
                        DSP-DEN010 NOT =   DSP-DEN008 AND
                        DSP-DEN010 NOT =   DSP-DEN009
                        MOVE  DSP-DEN010   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN011         NUMERIC ) AND
                      ( DSP-DEN011 NOT =   ZERO    )
                    IF  DSP-DEN011 NOT =   DSP-DEN001 AND
                        DSP-DEN011 NOT =   DSP-DEN002 AND
                        DSP-DEN011 NOT =   DSP-DEN003 AND
                        DSP-DEN011 NOT =   DSP-DEN004 AND
                        DSP-DEN011 NOT =   DSP-DEN005 AND
                        DSP-DEN011 NOT =   DSP-DEN006 AND
                        DSP-DEN011 NOT =   DSP-DEN007 AND
                        DSP-DEN011 NOT =   DSP-DEN008 AND
                        DSP-DEN011 NOT =   DSP-DEN009 AND
                        DSP-DEN011 NOT =   DSP-DEN010
                        MOVE  DSP-DEN011   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN012         NUMERIC ) AND
                      ( DSP-DEN012 NOT =   ZERO    )
                    IF  DSP-DEN012 NOT =   DSP-DEN001 AND
                        DSP-DEN012 NOT =   DSP-DEN002 AND
                        DSP-DEN012 NOT =   DSP-DEN003 AND
                        DSP-DEN012 NOT =   DSP-DEN004 AND
                        DSP-DEN012 NOT =   DSP-DEN005 AND
                        DSP-DEN012 NOT =   DSP-DEN006 AND
                        DSP-DEN012 NOT =   DSP-DEN007 AND
                        DSP-DEN012 NOT =   DSP-DEN008 AND
                        DSP-DEN012 NOT =   DSP-DEN009 AND
                        DSP-DEN012 NOT =   DSP-DEN010 AND
                        DSP-DEN012 NOT =   DSP-DEN011
                        MOVE  DSP-DEN012   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN013         NUMERIC ) AND
                      ( DSP-DEN013 NOT =   ZERO    )
                    IF  DSP-DEN013 NOT =   DSP-DEN001 AND
                        DSP-DEN013 NOT =   DSP-DEN002 AND
                        DSP-DEN013 NOT =   DSP-DEN003 AND
                        DSP-DEN013 NOT =   DSP-DEN004 AND
                        DSP-DEN013 NOT =   DSP-DEN005 AND
                        DSP-DEN013 NOT =   DSP-DEN006 AND
                        DSP-DEN013 NOT =   DSP-DEN007 AND
                        DSP-DEN013 NOT =   DSP-DEN008 AND
                        DSP-DEN013 NOT =   DSP-DEN009 AND
                        DSP-DEN013 NOT =   DSP-DEN010 AND
                        DSP-DEN013 NOT =   DSP-DEN011 AND
                        DSP-DEN013 NOT =   DSP-DEN012
                        MOVE  DSP-DEN013   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN014         NUMERIC ) AND
                      ( DSP-DEN014 NOT =   ZERO    )
                    IF  DSP-DEN014 NOT =   DSP-DEN001 AND
                        DSP-DEN014 NOT =   DSP-DEN002 AND
                        DSP-DEN014 NOT =   DSP-DEN003 AND
                        DSP-DEN014 NOT =   DSP-DEN004 AND
                        DSP-DEN014 NOT =   DSP-DEN005 AND
                        DSP-DEN014 NOT =   DSP-DEN006 AND
                        DSP-DEN014 NOT =   DSP-DEN007 AND
                        DSP-DEN014 NOT =   DSP-DEN008 AND
                        DSP-DEN014 NOT =   DSP-DEN009 AND
                        DSP-DEN014 NOT =   DSP-DEN010 AND
                        DSP-DEN014 NOT =   DSP-DEN011 AND
                        DSP-DEN014 NOT =   DSP-DEN012 AND
                        DSP-DEN014 NOT =   DSP-DEN013
                        MOVE  DSP-DEN014   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN015         NUMERIC )  AND
                      ( DSP-DEN015 NOT =   ZERO    )
                    IF  DSP-DEN015 NOT =   DSP-DEN001 AND
                        DSP-DEN015 NOT =   DSP-DEN002 AND
                        DSP-DEN015 NOT =   DSP-DEN003 AND
                        DSP-DEN015 NOT =   DSP-DEN004 AND
                        DSP-DEN015 NOT =   DSP-DEN005 AND
                        DSP-DEN015 NOT =   DSP-DEN006 AND
                        DSP-DEN015 NOT =   DSP-DEN007 AND
                        DSP-DEN015 NOT =   DSP-DEN008 AND
                        DSP-DEN015 NOT =   DSP-DEN009 AND
                        DSP-DEN015 NOT =   DSP-DEN010 AND
                        DSP-DEN015 NOT =   DSP-DEN011 AND
                        DSP-DEN015 NOT =   DSP-DEN012 AND
                        DSP-DEN015 NOT =   DSP-DEN013 AND
                        DSP-DEN015 NOT =   DSP-DEN014
                        MOVE  DSP-DEN015   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN016         NUMERIC )  AND
                      ( DSP-DEN016 NOT =   ZERO    )
                    IF  DSP-DEN016 NOT =   DSP-DEN001 AND
                        DSP-DEN016 NOT =   DSP-DEN002 AND
                        DSP-DEN016 NOT =   DSP-DEN003 AND
                        DSP-DEN016 NOT =   DSP-DEN004 AND
                        DSP-DEN016 NOT =   DSP-DEN005 AND
                        DSP-DEN016 NOT =   DSP-DEN006 AND
                        DSP-DEN016 NOT =   DSP-DEN007 AND
                        DSP-DEN016 NOT =   DSP-DEN008 AND
                        DSP-DEN016 NOT =   DSP-DEN009 AND
                        DSP-DEN016 NOT =   DSP-DEN010 AND
                        DSP-DEN016 NOT =   DSP-DEN011 AND
                        DSP-DEN016 NOT =   DSP-DEN012 AND
                        DSP-DEN016 NOT =   DSP-DEN013 AND
                        DSP-DEN016 NOT =   DSP-DEN014 AND
                        DSP-DEN016 NOT =   DSP-DEN015
                        MOVE  DSP-DEN016   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN017         NUMERIC )  AND
                      ( DSP-DEN017 NOT =   ZERO    )
                    IF  DSP-DEN017 NOT =   DSP-DEN001 AND
                        DSP-DEN017 NOT =   DSP-DEN002 AND
                        DSP-DEN017 NOT =   DSP-DEN003 AND
                        DSP-DEN017 NOT =   DSP-DEN004 AND
                        DSP-DEN017 NOT =   DSP-DEN005 AND
                        DSP-DEN017 NOT =   DSP-DEN006 AND
                        DSP-DEN017 NOT =   DSP-DEN007 AND
                        DSP-DEN017 NOT =   DSP-DEN008 AND
                        DSP-DEN017 NOT =   DSP-DEN009 AND
                        DSP-DEN017 NOT =   DSP-DEN010 AND
                        DSP-DEN017 NOT =   DSP-DEN011 AND
                        DSP-DEN017 NOT =   DSP-DEN012 AND
                        DSP-DEN017 NOT =   DSP-DEN013 AND
                        DSP-DEN017 NOT =   DSP-DEN014 AND
                        DSP-DEN017 NOT =   DSP-DEN015 AND
                        DSP-DEN017 NOT =   DSP-DEN016
                        MOVE  DSP-DEN017   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN018         NUMERIC )  AND
                      ( DSP-DEN018 NOT =   ZERO    )
                    IF  DSP-DEN018 NOT =   DSP-DEN001 AND
                        DSP-DEN018 NOT =   DSP-DEN002 AND
                        DSP-DEN018 NOT =   DSP-DEN003 AND
                        DSP-DEN018 NOT =   DSP-DEN004 AND
                        DSP-DEN018 NOT =   DSP-DEN005 AND
                        DSP-DEN018 NOT =   DSP-DEN006 AND
                        DSP-DEN018 NOT =   DSP-DEN007 AND
                        DSP-DEN018 NOT =   DSP-DEN008 AND
                        DSP-DEN018 NOT =   DSP-DEN009 AND
                        DSP-DEN018 NOT =   DSP-DEN010 AND
                        DSP-DEN018 NOT =   DSP-DEN011 AND
                        DSP-DEN018 NOT =   DSP-DEN012 AND
                        DSP-DEN018 NOT =   DSP-DEN013 AND
                        DSP-DEN018 NOT =   DSP-DEN014 AND
                        DSP-DEN018 NOT =   DSP-DEN015 AND
                        DSP-DEN018 NOT =   DSP-DEN016 AND
                        DSP-DEN018 NOT =   DSP-DEN017
                        MOVE  DSP-DEN018   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN019         NUMERIC )  AND
                      ( DSP-DEN019 NOT =   ZERO    )
                    IF  DSP-DEN019 NOT =   DSP-DEN001 AND
                        DSP-DEN019 NOT =   DSP-DEN002 AND
                        DSP-DEN019 NOT =   DSP-DEN003 AND
                        DSP-DEN019 NOT =   DSP-DEN004 AND
                        DSP-DEN019 NOT =   DSP-DEN005 AND
                        DSP-DEN019 NOT =   DSP-DEN006 AND
                        DSP-DEN019 NOT =   DSP-DEN007 AND
                        DSP-DEN019 NOT =   DSP-DEN008 AND
                        DSP-DEN019 NOT =   DSP-DEN009 AND
                        DSP-DEN019 NOT =   DSP-DEN010 AND
                        DSP-DEN019 NOT =   DSP-DEN011 AND
                        DSP-DEN019 NOT =   DSP-DEN012 AND
                        DSP-DEN019 NOT =   DSP-DEN013 AND
                        DSP-DEN019 NOT =   DSP-DEN014 AND
                        DSP-DEN019 NOT =   DSP-DEN015 AND
                        DSP-DEN019 NOT =   DSP-DEN016 AND
                        DSP-DEN019 NOT =   DSP-DEN017 AND
                        DSP-DEN019 NOT =   DSP-DEN018
                        MOVE  DSP-DEN019   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN020         NUMERIC )  AND
                      ( DSP-DEN020 NOT =   ZERO    )
                    IF  DSP-DEN020 NOT =   DSP-DEN001 AND
                        DSP-DEN020 NOT =   DSP-DEN002 AND
                        DSP-DEN020 NOT =   DSP-DEN003 AND
                        DSP-DEN020 NOT =   DSP-DEN004 AND
                        DSP-DEN020 NOT =   DSP-DEN005 AND
                        DSP-DEN020 NOT =   DSP-DEN006 AND
                        DSP-DEN020 NOT =   DSP-DEN007 AND
                        DSP-DEN020 NOT =   DSP-DEN008 AND
                        DSP-DEN020 NOT =   DSP-DEN009 AND
                        DSP-DEN020 NOT =   DSP-DEN010 AND
                        DSP-DEN020 NOT =   DSP-DEN011 AND
                        DSP-DEN020 NOT =   DSP-DEN012 AND
                        DSP-DEN020 NOT =   DSP-DEN013 AND
                        DSP-DEN020 NOT =   DSP-DEN014 AND
                        DSP-DEN020 NOT =   DSP-DEN015 AND
                        DSP-DEN020 NOT =   DSP-DEN016 AND
                        DSP-DEN020 NOT =   DSP-DEN017 AND
                        DSP-DEN020 NOT =   DSP-DEN018 AND
                        DSP-DEN020 NOT =   DSP-DEN019
                        MOVE  DSP-DEN020   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                MOVE    WK-DEN             TO   LINK-DENNO
*T↓
*               DISPLAY "LINK-DENNO=" LINK-DENNO  UPON CONS
*T↑
                MOVE    "END"              TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "5"      TO   PSW
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
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "5"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      確認
         WHEN   "6"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY94101"          TO   DSP-FMT.
     WRITE    DSP-FSY94101.
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
*　　　バッチ_
         WHEN   "1"
                MOVE    "GRP001"  TO   DSP-GRP
*　　　倉庫コード
         WHEN   "2"
                MOVE    "SOKCD"   TO   DSP-GRP
*　　　変更前納品日
         WHEN   "3"
                MOVE    "FDATE"   TO   DSP-GRP
*　　　変更後納品日
         WHEN   "4"
                MOVE    "TDATE"   TO   DSP-GRP
*　　　伝票番号
         WHEN   "5"
                MOVE    "GRP002"  TO   DSP-GRP
*　　　確認
         WHEN   "6"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY94101"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY94101.
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
***  バッチ_（日付）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTYMD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTYMD.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTOKC.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTOKC.
***  倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD.
***  変更前納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-FDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-FDATE.
***  変更後納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TDATE.
***  倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DEN001
                          EDIT-OPTION  OF  DSP-DEN002
                          EDIT-OPTION  OF  DSP-DEN003
                          EDIT-OPTION  OF  DSP-DEN004
                          EDIT-OPTION  OF  DSP-DEN005
                          EDIT-OPTION  OF  DSP-DEN006
                          EDIT-OPTION  OF  DSP-DEN007
                          EDIT-OPTION  OF  DSP-DEN008
                          EDIT-OPTION  OF  DSP-DEN009
                          EDIT-OPTION  OF  DSP-DEN010
                          EDIT-OPTION  OF  DSP-DEN011
                          EDIT-OPTION  OF  DSP-DEN012
                          EDIT-OPTION  OF  DSP-DEN013
                          EDIT-OPTION  OF  DSP-DEN014
                          EDIT-OPTION  OF  DSP-DEN015
                          EDIT-OPTION  OF  DSP-DEN016
                          EDIT-OPTION  OF  DSP-DEN017
                          EDIT-OPTION  OF  DSP-DEN018
                          EDIT-OPTION  OF  DSP-DEN019
                          EDIT-OPTION  OF  DSP-DEN020.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DEN001
                          EDIT-CURSOR  OF  DSP-DEN002
                          EDIT-CURSOR  OF  DSP-DEN003
                          EDIT-CURSOR  OF  DSP-DEN004
                          EDIT-CURSOR  OF  DSP-DEN005
                          EDIT-CURSOR  OF  DSP-DEN006
                          EDIT-CURSOR  OF  DSP-DEN007
                          EDIT-CURSOR  OF  DSP-DEN008
                          EDIT-CURSOR  OF  DSP-DEN009
                          EDIT-CURSOR  OF  DSP-DEN010
                          EDIT-CURSOR  OF  DSP-DEN011
                          EDIT-CURSOR  OF  DSP-DEN012
                          EDIT-CURSOR  OF  DSP-DEN013
                          EDIT-CURSOR  OF  DSP-DEN014
                          EDIT-CURSOR  OF  DSP-DEN015
                          EDIT-CURSOR  OF  DSP-DEN016
                          EDIT-CURSOR  OF  DSP-DEN017
                          EDIT-CURSOR  OF  DSP-DEN018
                          EDIT-CURSOR  OF  DSP-DEN019
                          EDIT-CURSOR  OF  DSP-DEN020.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
*    CLOSE             SHTDENF  HTOKMS  ZSOKMS1  DSPFILE.
     CLOSE             HTOKMS  ZSOKMS1  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY9410I   END PROGRAM  >>******************

```
