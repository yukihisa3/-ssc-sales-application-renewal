****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　        *
*    モジュール名　　　　：　共通受領書発行指示　　　　　　　　*
*    　　　　　　　　　　：　（出力指示）　　　　　　　　　　　*
*    作成日／作成者　　　：　2017/09/15 INOUE                  *
*    処理概要　　　　　　：　リスト発行指示　　　　　　　　　　*
*    流用　　　　　　　　：　SFU3080I + SSF0020I               *
*                                                              *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
*                          　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0240I.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2017/09/15.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*取引先マスタ
     SELECT  TOKMS2    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
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
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  TOKMS2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      TOKMS2    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJR02401  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SJR0240I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FJR02401".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SOKKBN-FLG               PIC  9(01)  VALUE  ZERO.
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
             VALUE NC"取引先コードを入力してください".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい日付を入力してください".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい区分を入力して下さい".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   6  PIC   N(20).
*
 01  FILE-ERR.
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
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
 01  LINK-OUT-TOKCD        PIC 9(08).
 01  LINK-OUT-DATEKBN      PIC X(01).
 01  LINK-OUT-FROMDATE     PIC 9(08).
 01  LINK-OUT-TODATE       PIC 9(08).
 01  LINK-OUT-HNPKBN       PIC X(01).
 01  LINK-OUT-OUTKBN       PIC X(01).
 01  LINK-OUT-PRTKBN       PIC X(01).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-OUT-TOKCD
                                     LINK-OUT-DATEKBN
                                     LINK-OUT-FROMDATE
                                     LINK-OUT-TODATE
                                     LINK-OUT-HNPKBN
                                     LINK-OUT-OUTKBN
                                     LINK-OUT-PRTKBN.
**************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKMS2.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
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
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     TOKMS2.
*ワークの初期化
     INITIALIZE         FLG-AREA
                        LINK-OUT-TOKCD
                        LINK-OUT-DATEKBN
                        LINK-OUT-FROMDATE
                        LINK-OUT-TODATE
                        LINK-OUT-HNPKBN
                        LINK-OUT-OUTKBN
                        LINK-OUT-PRTKBN.
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
*取引先ＣＤ
         WHEN      "1"  PERFORM   DSP-PARA1-SEC
*日付区分
         WHEN      "2"  PERFORM   DSP-PARA2-SEC
*日付指定
         WHEN      "J"  PERFORM   DSP-PARAD-SEC
         WHEN      "K"  PERFORM   DSP-PARAD-SEC
*返品区分
         WHEN      "3"  PERFORM   DSP-PARA3-SEC
*発行区分
         WHEN      "4"  PERFORM   DSP-PARA4-SEC
*出力区分
         WHEN      "5"  PERFORM   DSP-PARA5-SEC
*確認入力
         WHEN      "6"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
*             　取引先ＣＤ　　　　　　　　　　　　　　　　     *
****************************************************************
 DSP-PARA1-SEC         SECTION.
     MOVE      "DSP-PARA1-SEC"     TO   S-NAME.
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
                MOVE     "END"    TO   END-FLG
                MOVE     "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE     "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE      5       TO   ERR-FLG
                GO                TO   DSP-PARA1-SEC
     END-EVALUATE.
*
 DSP-PARA1-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK1-SEC             SECTION.
     MOVE    "PARA-CHK1-SEC"      TO   S-NAME.
*
*取引先コードチェック
***  未入力時，エラー
     IF     ( DSP-TOKCD  NOT NUMERIC )  OR
            ( DSP-TOKCD  =   ZERO    )
*             MOVE    SPACE        TO   DSP-TOKCD
              MOVE    1            TO   ERR-FLG
              MOVE   "R"           TO   EDIT-OPTION  OF  DSP-TOKCD
              MOVE   "C"           TO   EDIT-CURSOR  OF  DSP-TOKCD
              GO                   TO   PARA-CHK1-EXIT
     ELSE
***  取引先マスタＲＥＡＤ
              MOVE    DSP-TOKCD    TO   TOK-F01
              READ    TOKMS2
              INVALID
                      MOVE   2     TO   ERR-FLG
                      MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TOKCD
                      MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TOKCD
                      GO           TO   PARA-CHK1-EXIT
              NOT INVALID
                      MOVE "M"     TO   EDIT-OPTION  OF DSP-TOKCD
                      MOVE SPACE   TO   EDIT-CURSOR  OF DSP-TOKCD
                      MOVE TOK-F02 TO   DSP-TOKNM
              END-READ
     END-IF.
*
     MOVE    "2"       TO    PSW.
*
 PARA-CHK1-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 2 )                2.1       *
*             　日付区分　　　　　　　　　　　　　　　　　     *
****************************************************************
 DSP-PARA2-SEC         SECTION.
     MOVE      "DSP-PARA2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN  "E000"
                PERFORM   PARA-CHK2-SEC
                EVALUATE  DSP-DTKBN
                  WHEN    " "
                    MOVE  "J"    TO    PSW
                    MOVE  SPACE  TO    DSP-MAS003  DSP-MAS004
*                   MOVE  "C"    TO    EDIT-CURSOR OF DSP-JYUST
                  WHEN    "1"
                    MOVE  "K"    TO    PSW
                    MOVE  SPACE  TO    DSP-MAS001  DSP-MAS002
*                   MOVE  "C"    TO    EDIT-CURSOR OF DSP-KEIST
                END-EVALUATE
*終了
         WHEN  "F005"
                MOVE   "END"   TO    END-FLG
                MOVE   "4010"  TO    PROGRAM-STATUS
*取消
         WHEN  "F004"
                MOVE    "1"    TO    PSW
                PERFORM  INIT-DSP-SEC
*項目戻し
         WHEN  "F006"
                MOVE    "1"    TO    PSW
         WHEN   OTHER
                MOVE     5     TO    ERR-FLG
                GO             TO    DSP-PARA2-SEC
     END-EVALUATE.
*
 DSP-PARA2-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック2                      2.1.2     *
*             　日付区分　　　　　　　　　　　　　　　　　     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE    "PARA-CHK2-SEC"     TO   S-NAME.
*
*    PERFORM    DSP-WRITE-SEC.
*    PERFORM    DSP-READ-SEC.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*日付区分チェック
     IF       DSP-DTKBN   =  " "  OR  "1"
              CONTINUE
     ELSE
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-DTKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-DTKBN
              GO             TO   PARA-CHK2-EXIT
     END-IF.
*
*    MOVE    "3"       TO    PSW.
*
 PARA-CHK2-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = J/K )              2.1       *
*             　受信日・計上日　　　　　　　　　　　　　       *
****************************************************************
 DSP-PARAD-SEC         SECTION.
     MOVE      "DSP-PARAD-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN  "E000"
                PERFORM   PARA-CHKD-SEC
*終了
         WHEN  "F005"
                MOVE     "END"    TO   END-FLG
                MOVE     "4010"   TO   PROGRAM-STATUS
*取消
         WHEN  "F004"
                MOVE     "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN  "F006"
                MOVE    "2"      TO   PSW
         WHEN  OTHER
                MOVE     5       TO   ERR-FLG
                GO       TO      DSP-PARAD-SEC
     END-EVALUATE.
*
 DSP-PARAD-EXIT.
     EXIT.
****************************************************************
*             パラメタチェックD                      2.1.3     *
*             　受信日・計上日　　　　　　　　　　　　　       *
****************************************************************
 PARA-CHKD-SEC             SECTION.
     MOVE      "PARA-CHKD-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM    DSP-SYOKI-SEC.
 PARA-CHKD-01.
     EVALUATE   DSP-DTKBN
*      受信日
       WHEN   " "
           IF ( DSP-JYUST  =  SPACE    ) OR
              ( DSP-JYUST  =  ZERO     ) OR
              ( DSP-JYUST  =  99999999 )
                MOVE   ZERO       TO       DSP-JYUST
           ELSE
                MOVE  "2"         TO       LINK-IN-KBN
                MOVE   DSP-JYUST  TO       LINK-IN-YMD8
                CALL  "SKYDTCKB"  USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
                IF     LINK-OUT-RET NOT =  ZERO
                       MOVE   3    TO   ERR-FLG
                       MOVE  "C"   TO   EDIT-CURSOR OF DSP-JYUST
                       MOVE  "R"   TO   EDIT-OPTION OF DSP-JYUST
                       GO          TO   PARA-CHKD-EXIT
                END-IF
           END-IF
*
           IF ( DSP-JYUED  =  SPACE    ) OR
              ( DSP-JYUED  =  ZERO     ) OR
              ( DSP-JYUED  =  99999999 )
                MOVE   99999999   TO       DSP-JYUED
           ELSE
                MOVE  "2"         TO       LINK-IN-KBN
                MOVE   DSP-JYUED  TO       LINK-IN-YMD8
                CALL  "SKYDTCKB"  USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
                IF     LINK-OUT-RET NOT =  ZERO
                       MOVE   3    TO   ERR-FLG
                       MOVE  "C"   TO   EDIT-CURSOR OF DSP-JYUED
                       MOVE  "R"   TO   EDIT-OPTION OF DSP-JYUED
                       GO          TO   PARA-CHKD-EXIT
                END-IF
           END-IF
*          日付大小チェック
           IF   DSP-JYUST  >  DSP-JYUED
                MOVE      4    TO   ERR-FLG
                MOVE     "R"   TO   EDIT-OPTION  OF  DSP-JYUST
                MOVE     "R"   TO   EDIT-OPTION  OF  DSP-JYUED
                MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-JYUST
                GO             TO   PARA-CHKD-EXIT
           END-IF
*      計上日
       WHEN   "1"
           IF ( DSP-KEIST  =  SPACE    ) OR
              ( DSP-KEIST  =  ZERO     ) OR
              ( DSP-KEIST  =  99999999 )
                MOVE   ZERO       TO       DSP-KEIST
           ELSE
                MOVE  "2"         TO       LINK-IN-KBN
                MOVE   DSP-KEIST  TO       LINK-IN-YMD8
                CALL  "SKYDTCKB"  USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
                IF     LINK-OUT-RET NOT =  ZERO
                       MOVE   3    TO   ERR-FLG
                       MOVE  "C"   TO   EDIT-CURSOR OF DSP-KEIST
                       MOVE  "R"   TO   EDIT-OPTION OF DSP-KEIST
                       GO          TO   PARA-CHKD-EXIT
                END-IF
           END-IF
*
           IF ( DSP-KEIED  =  SPACE    ) OR
              ( DSP-KEIED  =  ZERO     ) OR
              ( DSP-KEIED  =  99999999 )
                MOVE   99999999   TO       DSP-KEIED
           ELSE
                MOVE  "2"         TO       LINK-IN-KBN
                MOVE   DSP-KEIED  TO       LINK-IN-YMD8
                CALL  "SKYDTCKB"  USING    LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8
                IF     LINK-OUT-RET NOT =  ZERO
                       MOVE   3    TO   ERR-FLG
                       MOVE  "C"   TO   EDIT-CURSOR OF DSP-KEIED
                       MOVE  "R"   TO   EDIT-OPTION OF DSP-KEIED
                       GO          TO   PARA-CHKD-EXIT
                END-IF
           END-IF
*          日付大小チェック
           IF   DSP-KEIST  >  DSP-KEIED
                MOVE      4    TO   ERR-FLG
                MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KEIST
                MOVE     "R"   TO   EDIT-OPTION  OF  DSP-KEIED
                MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-KEIST
                GO             TO   PARA-CHKD-EXIT
           END-IF
     END-EVALUATE.
*
 PARA-CHKD-02.
     MOVE    "3"       TO    PSW.
*
 PARA-CHKD-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 3 )                2.1       *
*             　返品区分　　　　　　　　　　　　　　　　　     *
****************************************************************
 DSP-PARA3-SEC         SECTION.
     MOVE      "DSP-PARA3-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN  "E000"
                PERFORM  PARA-CHK3-SEC
*終了
         WHEN  "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN  "F004"
                MOVE    "1"      TO   PSW
                PERFORM  INIT-DSP-SEC
*項目戻し
         WHEN  "F006"
                EVALUATE  DSP-DTKBN
                  WHEN    " "
                    MOVE  "J"    TO   PSW
*                   MOVE  "C"    TO   EDIT-CURSOR OF DSP-JYUST
                  WHEN    "1"
                    MOVE  "K"    TO   PSW
*                   MOVE  "C"    TO   EDIT-CURSOR OF DSP-KEIST
                END-EVALUATE
         WHEN   OTHER
                MOVE       5     TO   ERR-FLG
                GO               TO   DSP-PARA3-SEC
     END-EVALUATE.
*
 DSP-PARA3-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック3                      2.1.2     *
*             　返品区分　　　　　　　　　　　　　　　　　     *
****************************************************************
 PARA-CHK3-SEC             SECTION.
     MOVE     "PARA-CHK3-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*返品区分チェック
     IF       DSP-HENKBN    =  " "  OR  "1"  OR  "2"
              CONTINUE
     ELSE
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HENKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HENKBN
              GO             TO   PARA-CHK3-EXIT
     END-IF.
     MOVE    "4"       TO    PSW.
*
 PARA-CHK3-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 4 )                2.1       *
*             　発行区分　　　　　　　　　　　　　　　　　     *
****************************************************************
 DSP-PARA4-SEC         SECTION.
     MOVE      "DSP-PARA4-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN  "E000"
                PERFORM  PARA-CHK4-SEC
*終了
         WHEN  "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN  "F004"
                MOVE    "1"      TO   PSW
                PERFORM  INIT-DSP-SEC
*項目戻し
         WHEN  "F006"
                MOVE    "3"      TO   PSW
         WHEN  OTHER
                MOVE     5       TO   ERR-FLG
                GO               TO   DSP-PARA4-SEC
     END-EVALUATE.
*
 DSP-PARA4-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック4                      2.1.2     *
*             　発行区分　　　　　　　　　　　　　　　　　     *
****************************************************************
 PARA-CHK4-SEC             SECTION.
     MOVE    "PARA-CHK4-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*発行区分チェック
     IF       DSP-OUTKBN    =  " "  OR  "1"
              CONTINUE
     ELSE
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-OUTKBN
              GO             TO   PARA-CHK4-EXIT
     END-IF.
*
     IF       DSP-OUTKBN    =  " "
              MOVE  "5"      TO   PSW
     ELSE
              MOVE  "6"      TO   PSW
     END-IF.
*
 PARA-CHK4-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 5 )                2.1       *
*             　出力区分　　　　　　　　　　　　　　　　　　   *
****************************************************************
 DSP-PARA5-SEC         SECTION.
     MOVE      "DSP-PARA5-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN  "E000"
                PERFORM  PARA-CHK5-SEC
*終了
         WHEN  "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN  "F004"
                MOVE    "1"      TO   PSW
                PERFORM  INIT-DSP-SEC
*項目戻し
         WHEN  "F006"
                MOVE    "4"      TO   PSW
         WHEN  OTHER
                MOVE     5       TO   ERR-FLG
                GO               TO   DSP-PARA5-SEC
     END-EVALUATE.
*
 DSP-PARA5-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック5                      2.1.2     *
*             　出力区分　　　　　　　　　　　　　　　　　　   *
****************************************************************
 PARA-CHK5-SEC             SECTION.
     MOVE    "PARA-CHK5-SEC"     TO   S-NAME.
*属性を初期化する
     PERFORM  DSP-SYOKI-SEC.
*出力区分チェック
     IF       DSP-PRTKBN    =  " "  OR  "1"
              CONTINUE
     ELSE
              MOVE   6       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-PRTKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-PRTKBN
              GO             TO   PARA-CHK5-EXIT
     END-IF.
     MOVE    "6"       TO    PSW.
*
 PARA-CHK5-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 6 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE      "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    DSP-TOKCD     TO   LINK-OUT-TOKCD
                MOVE    DSP-DTKBN     TO   LINK-OUT-DATEKBN
                IF      DSP-DTKBN     =    " "
                        MOVE    DSP-JYUST  TO  LINK-OUT-FROMDATE
                        MOVE    DSP-JYUED  TO  LINK-OUT-TODATE
                ELSE
                        MOVE    DSP-KEIST  TO  LINK-OUT-FROMDATE
                        MOVE    DSP-KEIED  TO  LINK-OUT-TODATE
                END-IF
                MOVE    DSP-HENKBN    TO   LINK-OUT-HNPKBN
                MOVE    DSP-OUTKBN    TO   LINK-OUT-OUTKBN
                MOVE    DSP-PRTKBN    TO   LINK-OUT-PRTKBN
                MOVE    "END"         TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                IF       DSP-OUTKBN = " "
                         MOVE   "5" TO   PSW
                ELSE
                         MOVE   "4" TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM  INIT-DSP-SEC
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
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
         WHEN   "J"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   "K"
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
     MOVE    "FJR02401"          TO   DSP-FMT.
     WRITE    DSP-FJR02401.
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
*取引先ＣＤ　
         WHEN   "1"
                MOVE    "PARGP1"  TO   DSP-GRP
*日付区分
         WHEN   "2"
                MOVE    "PARGP2"  TO   DSP-GRP
*受信日
         WHEN   "J"
                MOVE    "PARGPJ"  TO   DSP-GRP
*計上日
         WHEN   "K"
                MOVE    "PARGPK"  TO   DSP-GRP
*返品区分
         WHEN   "3"
                MOVE    "PARGP3"  TO   DSP-GRP
*発行区分
         WHEN   "4"
                MOVE    "PARGP4"  TO   DSP-GRP
*出力区分
         WHEN   "5"
                MOVE    "PARGP5"  TO   DSP-GRP
*確認
         WHEN   "6"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE   "FJR02401"            TO   DSP-FMT.
     READ    DSPFILE.
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
     MOVE    SPACE                TO   DSP-FJR02401.
*プログラムＩＤ転送
     MOVE    WORK-PGID            TO   DSP-PGID.
*ＦＯＲＭＩＤ転送
     MOVE    WORK-FORMID          TO   DSP-FORMID.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  取引先コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TOKCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TOKCD.
***  ＤＴ区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DTKBN.
***  受信日（開始）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JYUST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JYUST.
***  受信日（終了）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JYUED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JYUED.
***  計上日（開始）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KEIST.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KEIST.
***  計上日（終了）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KEIED.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KEIED.
***  返品区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HENKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HENKBN.
***  発行区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-OUTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-OUTKBN.
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-PRTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-PRTKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             TOKMS2  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SJR0240I   END PROGRAM  >>******************
