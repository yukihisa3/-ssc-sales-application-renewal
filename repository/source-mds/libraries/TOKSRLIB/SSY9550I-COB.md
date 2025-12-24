# SSY9550I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9550I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　          *
*    サブシステム　　　　：　ＥＤＩ　　　　　　　　　　　　　　*
*    モジュール名　　　　：　ヨドバシ出荷確定取消指示　　　　　*
*    作成日／作成者　　　：　2023/01/19 INOUE                  *
*    処理概要　　　　　　：　一括取消条件指定　　　　　　　　　*
*    　　　　　　　　　　：　　　　　　                        *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　                    *
*    　　　　　　　　　　：　　　　　　                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY9550I.
*                 流用:SSY9410I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2023/01/19.
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
*----<< 基本情報ファイル >>--*
     SELECT   YODJOHL4  ASSIGN         DA-01-VI-YODJOHL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F15
                                       JOH-F16
                                       JOH-F17
                                       JOH-F28
                                       JOH-F14
                                       JOH-F18
                                       JOH-F19
                        STATUS         JOH-ST.
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
*----<< 基本情報ファイル >>--*
 FD  YODJOHL4            LABEL     RECORD   IS   STANDARD.
     COPY     YODJOHL4   OF        XFDLIB
              JOINING    JOH       PREFIX.
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
                       COPY      FSY95501  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  JOH-ST                   PIC  X(02).
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
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY9550I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY95501".
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
             VALUE NC"既に本社確定済の伝票があります".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"納品日を入力してください".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"基本情報ファイルに登録されていません".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"納品日　論理エラー".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"伝票番号を入力してください".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"未確定の伝票番号が入力されています".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  12  PIC   N(20).
*
 01  FILE-ERR.
     03  JOH-ERR           PIC N(20) VALUE
                        NC"基本情報ファイルエラー".
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
 01  LINK-DSOKO          PIC  X(02).
 01  LINK-TAISYO         PIC  X(01).
 01  LINK-DATE           PIC  9(08).
 01  LINK-TIME           PIC  9(04).
 01  LINK-TORICD         PIC  X(08).
 01  LINK-SOKCD          PIC  X(02).
 01  LINK-NDATE          PIC  9(08).
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
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-DSOKO
                                     LINK-TAISYO
                                     LINK-DATE
                                     LINK-TIME
                                     LINK-TORICD
                                     LINK-SOKCD
                                     LINK-NDATE
                                     LINK-DENNO.
**************************************************************
 DECLARATIVES.
 JOH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE YODJOHL4.
     DISPLAY     JOH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JOH-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
     OPEN     INPUT     YODJOHL4  HTOKMS   ZSOKMS1.
*
*ワークの初期化
     INITIALIZE         FLG-AREA.
     INITIALIZE         LINK-TAISYO
                        LINK-DATE
                        LINK-TIME
                        LINK-TORICD
                        LINK-SOKCD
                        LINK-NDATE
                        LINK-DENNO-01
                        LINK-DENNO-02
                        LINK-DENNO-03
                        LINK-DENNO-04
                        LINK-DENNO-05
                        LINK-DENNO-06
                        LINK-DENNO-07
                        LINK-DENNO-08
                        LINK-DENNO-09
                        LINK-DENNO-10.
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
*---     WHEN      "5"  PERFORM   DSP-PARA-SEC
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
*               IF       PSW     =    "5"
*                        MOVE   "4"   TO   PSW
*               END-IF
                IF       PSW     =    "6"
                         MOVE   "4"   TO   PSW
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
*    IF       PSW   =  "5"
*             GO             TO   PARA-CHK-05
*    END-IF.
*
 PARA-CHK-01.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-JDATE  NOT NUMERIC
         OR   DSP-JDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-JDATE      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-JDATE
     END-IF.
*
*バッチ_（時間）チェック
***  バッチ_（時間）未入力チェック
     IF       DSP-JTIME  NOT NUMERIC
         OR   DSP-JTIME  =   ZERO
                   MOVE   1       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-JTIME
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-JTIME
                   GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（時間）論理チェック
              MOVE     DSP-JTIME    TO  WK-JIKAN
              IF       WK-HH  <=  ZERO  OR  WK-HH  >  24 OR
                       WK-MM  <   ZERO  OR  WK-MM  >  59
                       IF     ERR-FLG  =  ZERO
                              MOVE    5    TO    ERR-FLG
                       END-IF
                       MOVE "R"    TO EDIT-OPTION OF DSP-JTIME
                       MOVE "C"    TO EDIT-CURSOR OF DSP-JTIME
                       GO          TO PARA-CHK-EXIT
              ELSE
                       MOVE "M"    TO EDIT-OPTION OF DSP-JTIME
                       MOVE SPACE  TO EDIT-CURSOR OF DSP-JTIME
              END-IF
     END-IF.
*
*バッチ_（取引先）チェック
***  バッチ_（取引先）未入力チェック
     IF       DSP-TORICD  NOT NUMERIC
         OR   DSP-TORICD  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TORICD
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TORICD
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-TORICD TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TORICD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TORICD
                     MOVE   SPACE TO   DSP-TORINM
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-TORICD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-TORICD
                     MOVE  TOK-F03  TO DSP-TORINM
              END-READ
     END-IF.
*
*売上伝票ファイル存在チェック
*    MOVE     SPACE          TO   DEN-REC.
*    INITIALIZE                   DEN-REC.
*    MOVE     DSP-JDATE      TO   DEN-F46.
*    MOVE     DSP-JTIME     TO   DEN-F47.
*    MOVE     DSP-TORICD     TO   DEN-F01.
*    MOVE     SPACE          TO   DEN-F48.
*    START    SHTDENF   KEY  IS   >=   DEN-F46  DEN-F47  DEN-F01
*                                      DEN-F48  DEN-F02  DEN-F04
*                                      DEN-F051 DEN-F07  DEN-F112
*                                      DEN-F03
*    INVALID
*             MOVE      9    TO   ERR-FLG
*             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-JDATE
*             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-JTIME
*             MOVE     "R"   TO   EDIT-OPTION  OF  DSP-TORICD
*             MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-JDATE
*             GO             TO   PARA-CHK-EXIT
*    NOT INVALID
*             売上ファイル存在チェック
*             READ     SHTDENF
*             AT END
*                  MOVE   9   TO   ERR-FLG
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-JDATE
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-JTIME
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-TORICD
*                  MOVE  "C"  TO   EDIT-CURSOR OF DSP-JDATE
*                  GO         TO   PARA-CHK-EXIT
*             END-READ
*             IF   DSP-JDATE   =   DEN-F46 AND
*                  DSP-JTIME  =   DEN-F47 AND
*                  DSP-TORICD  =   DEN-F01
*                  MOVE  SPACE TO  EDIT-CURSOR OF DSP-JDATE
*             ELSE
*                  MOVE   9   TO   ERR-FLG
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-JDATE
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-JTIME
*                  MOVE  "R"  TO   EDIT-OPTION OF DSP-TORICD
*                  MOVE  "C"  TO   EDIT-CURSOR OF DSP-JDATE
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
     IF       DSP-SOKO  NOT = SPACE
              MOVE       DSP-SOKO  TO  SOK-F01
              PERFORM    ZSOKMS1-READ-SEC
              IF         ZSOKMS1-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKO
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKO
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKO
                   END-IF
              ELSE
                         MOVE  SOK-F02              TO DSP-SOKONM
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SOKO
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SOKO
              END-IF
     ELSE
*             MOVE  "M"  TO  EDIT-OPTION OF DSP-SOKO
*             MOVE  " "  TO  EDIT-CURSOR OF DSP-SOKO
              IF    ERR-FLG     = ZERO
                    MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKO
                    MOVE  "C"  TO  EDIT-CURSOR OF DSP-SOKO
                    MOVE   7   TO  ERR-FLG
              ELSE
                    MOVE  "R"  TO  EDIT-OPTION OF DSP-SOKO
              END-IF
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "3"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
 PARA-CHK-03.
*納品日チェック
*    未入力チェック
     IF       DSP-NDATE  NOT NUMERIC
         OR   DSP-NDATE  =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
*             論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NDATE      TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  10       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NDATE
     END-IF.
*
     IF       ERR-FLG  =  0
              MOVE    "4"     TO   PSW
     END-IF.
     GO                TO    PARA-CHK-99.
*
*PARA-CHK-04.
*なし
*    未入力チェック
*    IF       DSP-TDATE  NOT NUMERIC
*        OR   DSP-TDATE  =  ZERO
*             MOVE   8       TO   ERR-FLG
*             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TDATE
*             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TDATE
*             GO             TO   PARA-CHK-EXIT
*    ELSE
*             論理チェック
*             MOVE     "2"            TO   LINK-IN-KBN
*             MOVE     ZERO           TO   LINK-IN-YMD6
*             MOVE     DSP-TDATE      TO   LINK-IN-YMD8
*             MOVE     ZERO           TO   LINK-OUT-RET
*             MOVE     ZERO           TO   LINK-OUT-YMD
*             CALL     "SKYDTCKB"     USING   LINK-IN-KBN
*                                             LINK-IN-YMD6
*                                             LINK-IN-YMD8
*                                             LINK-OUT-RET
*                                             LINK-OUT-YMD
*             IF   LINK-OUT-RET   = 9
*                  MOVE  10       TO   ERR-FLG
*                  MOVE  "R"      TO   EDIT-OPTION  OF  DSP-TDATE
*                  MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-TDATE
*                  GO             TO   PARA-CHK-EXIT
*             END-IF
*
*             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-TDATE
*             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-TDATE
*    END-IF.
*
*    IF       ERR-FLG  =  0
*             MOVE    "5"     TO   PSW
*    END-IF.
*    GO                TO    PARA-CHK-99.
*
 PARA-CHK-04.
*伝票番号チェック
     MOVE     ZERO                TO   ERR-FLG.
*1
*    IF     ( DSP-DEN01 NOT NUMERIC ) OR
*           ( DSP-DEN01 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN01
*    END-IF.
*2
*    IF     ( DSP-DEN02 NOT NUMERIC ) OR
*           ( DSP-DEN02 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN02
*    END-IF.
*3
*    IF     ( DSP-DEN03 NOT NUMERIC ) OR
*           ( DSP-DEN03 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN03
*    END-IF.
*4
*    IF     ( DSP-DEN04 NOT NUMERIC ) OR
*           ( DSP-DEN04 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN04
*    END-IF.
*5
*    IF     ( DSP-DEN05 NOT NUMERIC ) OR
*           ( DSP-DEN05 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN05
*    END-IF.
*6
*    IF     ( DSP-DEN06 NOT NUMERIC ) OR
*           ( DSP-DEN06 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN06
*    END-IF.
*7
*    IF     ( DSP-DEN07 NOT NUMERIC ) OR
*           ( DSP-DEN07 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN07
*    END-IF.
*8
*    IF     ( DSP-DEN08 NOT NUMERIC ) OR
*           ( DSP-DEN08 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN08
*    END-IF.
*9
*    IF     ( DSP-DEN09 NOT NUMERIC ) OR
*           ( DSP-DEN09 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN09
*    END-IF.
*10
*    IF     ( DSP-DEN10 NOT NUMERIC ) OR
*           ( DSP-DEN10 =  ZERO     )
*             MOVE       ZERO       TO  DSP-DEN10
*    END-IF.
*
 PARA-CHK-040.
     IF     (( DSP-DEN01 NOT NUMERIC ) OR ( DSP-DEN01 =  ZERO ))
        AND (( DSP-DEN02 NOT NUMERIC ) OR ( DSP-DEN02 =  ZERO ))
        AND (( DSP-DEN03 NOT NUMERIC ) OR ( DSP-DEN03 =  ZERO ))
        AND (( DSP-DEN04 NOT NUMERIC ) OR ( DSP-DEN04 =  ZERO ))
        AND (( DSP-DEN05 NOT NUMERIC ) OR ( DSP-DEN05 =  ZERO ))
        AND (( DSP-DEN06 NOT NUMERIC ) OR ( DSP-DEN06 =  ZERO ))
        AND (( DSP-DEN07 NOT NUMERIC ) OR ( DSP-DEN07 =  ZERO ))
        AND (( DSP-DEN08 NOT NUMERIC ) OR ( DSP-DEN08 =  ZERO ))
        AND (( DSP-DEN09 NOT NUMERIC ) OR ( DSP-DEN09 =  ZERO ))
        AND (( DSP-DEN10 NOT NUMERIC ) OR ( DSP-DEN10 =  ZERO ))
            MOVE  11       TO   ERR-FLG
            MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DEN01
            MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DEN01
            GO             TO   PARA-CHK-EXIT
     END-IF.
*
 PARA-CHK-041.
     IF     ( DSP-DEN01  NUMERIC ) AND ( DSP-DEN01 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN01   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN01
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN01
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN01
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN01
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN01
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN01
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN01
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN01
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-042.
     IF     ( DSP-DEN02  NUMERIC ) AND ( DSP-DEN02 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN02   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN02
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN02
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN02
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN02
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN02
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN02
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN02
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN02
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-043.
     IF     ( DSP-DEN03  NUMERIC ) AND ( DSP-DEN03 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN03   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN03
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN03
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN03
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN03
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN03
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN03
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN03
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN03
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-044.
     IF     ( DSP-DEN04  NUMERIC ) AND ( DSP-DEN04 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN04   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN04
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN04
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN04
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN04
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN04
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN04
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN04
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN04
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-045.
     IF     ( DSP-DEN05  NUMERIC ) AND ( DSP-DEN05 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN05   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN05
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN05
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN05
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN05
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN05
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN05
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN05
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN05
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-046.
     IF     ( DSP-DEN06  NUMERIC ) AND ( DSP-DEN06 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN06   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN06
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN06
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN06
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN06
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN06
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN06
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN06
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN06
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-047.
     IF     ( DSP-DEN07  NUMERIC ) AND ( DSP-DEN07 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN07   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN07
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN07
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN07
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN07
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN07
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN07
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN07
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN07
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-048.
     IF     ( DSP-DEN08  NUMERIC ) AND ( DSP-DEN08 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN08   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN08
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN08
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN08
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN08
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN08
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN08
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN08
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN08
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-049.
     IF     ( DSP-DEN09  NUMERIC ) AND ( DSP-DEN09 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN09   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN09
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN09
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN09
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN09
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN09
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN09
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN09
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN09
                  END-IF
              END-IF
     END-IF.
 PARA-CHK-04A.
     IF     ( DSP-DEN10  NUMERIC ) AND ( DSP-DEN10 NOT = ZERO )
              MOVE     DSP-JDATE   TO    JOH-F15
              MOVE     DSP-JTIME   TO    JOH-F16
              MOVE     DSP-TORICD  TO    JOH-F17
              MOVE     DSP-SOKO    TO    JOH-F28
              MOVE     DSP-NDATE   TO    JOH-F14
              MOVE     DSP-DEN10   TO    JOH-F18
              MOVE     1           TO    JOH-F19
              READ     YODJOHL4
                INVALID
                       MOVE  9     TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN10
                       GO        TO   PARA-CHK-EXIT
              END-READ
              IF       LINK-DSOKO  =  "01"
                  IF   JOH-F26 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN10
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN10
                  END-IF
              ELSE
                  IF   JOH-F34 = 0
                       MOVE  12    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN10
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN10
                  END-IF
                  IF   JOH-F26 NOT = 0
                       MOVE   6    TO    ERR-FLG
                       MOVE  "R" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  "C" TO   EDIT-CURSOR  OF  DSP-DEN10
                       GO        TO   PARA-CHK-EXIT
                  ELSE
                       MOVE  "M" TO   EDIT-OPTION  OF  DSP-DEN10
                       MOVE  " " TO   EDIT-CURSOR  OF  DSP-DEN10
                  END-IF
              END-IF
     END-IF.
*
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
                MOVE    DSP-CANKBN    TO   LINK-TAISYO
                MOVE    DSP-JDATE     TO   LINK-DATE
                MOVE    DSP-JTIME     TO   LINK-TIME
                MOVE    DSP-TORICD    TO   LINK-TORICD
                MOVE    DSP-SOKO      TO   LINK-SOKCD
                MOVE    DSP-NDATE     TO   LINK-NDATE
                MOVE    ALL "0"       TO   WK-DEN
                MOVE    1             TO   IX
                IF    ( DSP-DEN01         NUMERIC ) AND
                      ( DSP-DEN01 NOT =   ZERO    )
                        MOVE  DSP-DEN01   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                END-IF
                IF    ( DSP-DEN02         NUMERIC ) AND
                      ( DSP-DEN02 NOT =   ZERO    )
                    IF  DSP-DEN02 NOT =   DSP-DEN01
                        MOVE  DSP-DEN02   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN03         NUMERIC ) AND
                      ( DSP-DEN03 NOT =   ZERO    )
                    IF  DSP-DEN03 NOT =   DSP-DEN01 AND
                        DSP-DEN03 NOT =   DSP-DEN02
                        MOVE  DSP-DEN03   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN04         NUMERIC ) AND
                      ( DSP-DEN04 NOT =   ZERO    )
                    IF  DSP-DEN04 NOT =   DSP-DEN01 AND
                        DSP-DEN04 NOT =   DSP-DEN02 AND
                        DSP-DEN04 NOT =   DSP-DEN03
                        MOVE  DSP-DEN04   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN05         NUMERIC ) AND
                      ( DSP-DEN05 NOT =   ZERO    )
                    IF  DSP-DEN05 NOT =   DSP-DEN01 AND
                        DSP-DEN05 NOT =   DSP-DEN02 AND
                        DSP-DEN05 NOT =   DSP-DEN03 AND
                        DSP-DEN05 NOT =   DSP-DEN04
                        MOVE  DSP-DEN05   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN06         NUMERIC ) AND
                      ( DSP-DEN06 NOT =   ZERO    )
                    IF  DSP-DEN06 NOT =   DSP-DEN01 AND
                        DSP-DEN06 NOT =   DSP-DEN02 AND
                        DSP-DEN06 NOT =   DSP-DEN03 AND
                        DSP-DEN06 NOT =   DSP-DEN04 AND
                        DSP-DEN06 NOT =   DSP-DEN05
                        MOVE  DSP-DEN06   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN07         NUMERIC ) AND
                      ( DSP-DEN07 NOT =   ZERO    )
                    IF  DSP-DEN07 NOT =   DSP-DEN01 AND
                        DSP-DEN07 NOT =   DSP-DEN02 AND
                        DSP-DEN07 NOT =   DSP-DEN03 AND
                        DSP-DEN07 NOT =   DSP-DEN04 AND
                        DSP-DEN07 NOT =   DSP-DEN05 AND
                        DSP-DEN07 NOT =   DSP-DEN06
                        MOVE  DSP-DEN07   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN08         NUMERIC ) AND
                      ( DSP-DEN08 NOT =   ZERO    )
                    IF  DSP-DEN08 NOT =   DSP-DEN01 AND
                        DSP-DEN08 NOT =   DSP-DEN02 AND
                        DSP-DEN08 NOT =   DSP-DEN03 AND
                        DSP-DEN08 NOT =   DSP-DEN04 AND
                        DSP-DEN08 NOT =   DSP-DEN05 AND
                        DSP-DEN08 NOT =   DSP-DEN06 AND
                        DSP-DEN08 NOT =   DSP-DEN07
                        MOVE  DSP-DEN08   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN09         NUMERIC ) AND
                      ( DSP-DEN09 NOT =   ZERO    )
                    IF  DSP-DEN09 NOT =   DSP-DEN01 AND
                        DSP-DEN09 NOT =   DSP-DEN02 AND
                        DSP-DEN09 NOT =   DSP-DEN03 AND
                        DSP-DEN09 NOT =   DSP-DEN04 AND
                        DSP-DEN09 NOT =   DSP-DEN05 AND
                        DSP-DEN09 NOT =   DSP-DEN06 AND
                        DSP-DEN09 NOT =   DSP-DEN07 AND
                        DSP-DEN09 NOT =   DSP-DEN08
                        MOVE  DSP-DEN09   TO   WK-DENNO(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF    ( DSP-DEN10         NUMERIC ) AND
                      ( DSP-DEN10 NOT =   ZERO    )
                    IF  DSP-DEN10 NOT =   DSP-DEN01 AND
                        DSP-DEN10 NOT =   DSP-DEN02 AND
                        DSP-DEN10 NOT =   DSP-DEN03 AND
                        DSP-DEN10 NOT =   DSP-DEN04 AND
                        DSP-DEN10 NOT =   DSP-DEN05 AND
                        DSP-DEN10 NOT =   DSP-DEN06 AND
                        DSP-DEN10 NOT =   DSP-DEN07 AND
                        DSP-DEN10 NOT =   DSP-DEN08 AND
                        DSP-DEN10 NOT =   DSP-DEN09
                        MOVE  DSP-DEN10   TO   WK-DENNO(IX)
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
                MOVE    "4"      TO   PSW
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
           MOVE    SPACE              TO   DSP-MSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-GUIDE
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
*        WHEN   "5"
*               MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      確認
         WHEN   "6"
                MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-GUIDE
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY95501"          TO   DSP-FMT.
     WRITE    DSP-FSY95501.
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
                MOVE    "GRP01"   TO   DSP-GRP
*　　　倉庫コード
         WHEN   "2"
                MOVE    "SOKO"    TO   DSP-GRP
*　　　納品日
         WHEN   "3"
                MOVE    "NDATE"   TO   DSP-GRP
*　　　伝票番号
         WHEN   "4"
                MOVE    "GRP002"  TO   DSP-GRP
*　　　確認
         WHEN   "6"
*---            MOVE    "ENDCHK"  TO   DSP-GRP
                MOVE    "KKNN"    TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY95501"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY95501.
*プログラムＩＤ転送
     MOVE    WORK-PGID            TO   DSP-PGID.
*ＦＯＲＭＩＤ転送
     MOVE    WORK-FORMID          TO   DSP-FORM.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SYSYMD.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-SYSTIM.
*項目属性クリア　
     PERFORM DSP-SYOKI-SEC.
*
     IF      LINK-DSOKO  =  "01"
             MOVE   "2"  TO  DSP-CANKBN
             MOVE   "X"  TO  EDIT-STATUS OF DSP-CANKBN
     ELSE
             MOVE   "1"  TO  DSP-CANKBN
             MOVE   "X"  TO  EDIT-STATUS OF DSP-CANKBN
     END-IF.
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
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JDATE.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD.
***  倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKO.
***  納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NDATE.
***  倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DEN01
                          EDIT-OPTION  OF  DSP-DEN02
                          EDIT-OPTION  OF  DSP-DEN03
                          EDIT-OPTION  OF  DSP-DEN04
                          EDIT-OPTION  OF  DSP-DEN05
                          EDIT-OPTION  OF  DSP-DEN06
                          EDIT-OPTION  OF  DSP-DEN07
                          EDIT-OPTION  OF  DSP-DEN08
                          EDIT-OPTION  OF  DSP-DEN09
                          EDIT-OPTION  OF  DSP-DEN10.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DEN01
                          EDIT-CURSOR  OF  DSP-DEN02
                          EDIT-CURSOR  OF  DSP-DEN03
                          EDIT-CURSOR  OF  DSP-DEN04
                          EDIT-CURSOR  OF  DSP-DEN05
                          EDIT-CURSOR  OF  DSP-DEN06
                          EDIT-CURSOR  OF  DSP-DEN07
                          EDIT-CURSOR  OF  DSP-DEN08
                          EDIT-CURSOR  OF  DSP-DEN09
                          EDIT-CURSOR  OF  DSP-DEN10.
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
*****************<<  SSY9550I   END PROGRAM  >>******************

```
