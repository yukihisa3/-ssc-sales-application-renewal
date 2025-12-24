****************************************************************
*    顧客名　　　　　　：（株）サカタのタネ殿　　　　　　　    *
*    サブシステム　　　：　受領返品取込機能構築　　　　        *
*    業務名　　　　　　：　伝票纏め返品　　　　　              *
*    モジュール名　　　：　返品累積伝票纏め処理実行指示　　　  *
*    作成日／更新日　　：　2017/08/28                          *
*    作成者／更新者　　：　NAV T.TAKAHASHI                     *
*    処理概要　　　　　：　画面より，バッチ_を入力し，パラメ  *
*                          タにバッチ番号を受け渡す。          *
*                          バッチ_が返品累積データに存在しない*
*                          場合はエラー。伝票纏め済はエラー。　*
*    更新日／更新者　　：　2017/11/21 INOUE                    *
*    更新概要　　　　　：　単体ＮＧ修正　　　　　　　　　　　  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJR0360I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2017/08/28.
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
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*返品累積データ
     SELECT  COMRHEF   ASSIGN    TO        COMRHEL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       RMH-F78
                                           RMH-F79
                                           RMH-F01
                                           RMH-F02
                                           RMH-F03
                                           RMH-F04
                                           RMH-F05
                       FILE      STATUS    RMH-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 伝票纏め返品累積データ                             *
****************************************************************
 FD  COMRHEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      COMRHEF   OF   XFDLIB
                       JOINING   RMH       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJR03601  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TOK-ST                   PIC  X(02).
     03  RMH-ST                   PIC  X(02).
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
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SONZAI-CHK               PIC  X(03)  VALUE  SPACE.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  COMRHEF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.

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
             VALUE NC"日付を正しく入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"時刻を正しく入力して下さい".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先ＣＤを入力して下さい".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"取引先ＣＤが未登録です".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"対象バッチが返品累積Ｆに存在しません".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"発行区分は空白又は１を指定して下さい".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キー！！".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"出力区分は空白又は１を指定して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"伝票纏めを行なう取引先ではありません".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"既に伝票纏め処理済のデータです。！".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  11  PIC   N(20).
*
 01  FILE-ERR.
     03  RMH-ERR           PIC N(20) VALUE
                        NC"返品累積データエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE               SECTION.
 01  PARA-HIDUKE           PIC 9(08).
 01  PARA-JIKAN            PIC 9(04).
 01  PARA-TORCD            PIC 9(08).
 01  PARA-HAKKBN           PIC X(01).
 01  PARA-SYUKBN           PIC X(01).
*
**************************************************************
**************************************************************
 PROCEDURE             DIVISION  USING
                                 PARA-HIDUKE
                                 PARA-JIKAN
                                 PARA-TORCD
                                 PARA-HAKKBN
                                 PARA-SYUKBN.
**************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RMH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE COMRHEF.
     DISPLAY     RMH-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     RMH-ST    UPON      CONS.
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
 PROCESS-EXIT.
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS  COMRHEF.
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
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
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
                IF       ERR-FLG = ZERO
                         MOVE   "2"   TO   PSW
                END-IF
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     8       TO   ERR-FLG
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
     MOVE      ZERO              TO   ERR-FLG.
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTDATE  NOT NUMERIC
         OR   DSP-BTDATE  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
              GO             TO   PARA-CHK-EXIT
     ELSE
***           バッチ_（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-BTDATE     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  2        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTDATE
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTDATE
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-BTDATE
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-BTDATE
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
                              MOVE    3    TO    ERR-FLG
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
     IF       DSP-BTTORI  NOT NUMERIC
         OR   DSP-BTTORI  =  ZERO
              MOVE   4       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTTORI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTTORI
              GO             TO   PARA-CHK-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTORI TO   TOK-F01
              PERFORM  HTOKMS-READ-SEC
              IF  HTOKMS-INV-FLG  =  "INV"
                  MOVE   5     TO   ERR-FLG
                  MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTORI
                  MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTORI
                  MOVE ALL NC"＊" TO   DSP-TORINM
                  GO           TO   PARA-CHK-EXIT
              ELSE
                IF  TOK-FIL1(1:1) = SPACE
                    MOVE   10    TO   ERR-FLG
                    MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTORI
                    MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTORI
                    MOVE TOK-F03 TO   DSP-TORINM
                    GO           TO   PARA-CHK-EXIT
                ELSE
                    MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTORI
                    MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTORI
                    MOVE  TOK-F03  TO DSP-TORINM
                END-IF
              END-IF
     END-IF.
*
*発行区分
     IF       DSP-HAKKBN  =  SPACE  OR  "1"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-HAKKBN
     ELSE
              MOVE   7       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-HAKKBN
              GO             TO   PARA-CHK-EXIT
     END-IF.
*
*出力区分
     IF       DSP-SYUKBN  =  SPACE  OR  "1"
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SYUKBN
     ELSE
              MOVE   9       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SYUKBN
              GO             TO   PARA-CHK-EXIT
     END-IF.
*伝票纏め返品累積データ存在チェック
     MOVE     SPACE          TO   RMH-REC.
     INITIALIZE                   RMH-REC.
     MOVE     DSP-BTDATE     TO   RMH-F78.
     MOVE     DSP-BTTIME     TO   RMH-F79.
     MOVE     DSP-BTTORI     TO   RMH-F01.
     START    COMRHEF   KEY  IS   >=   RMH-F78  RMH-F79  RMH-F01
                                       RMH-F02  RMH-F03  RMH-F04
                                       RMH-F05
     INVALID
              MOVE      6    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTDATE
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTDATE
              GO             TO   PARA-CHK-EXIT
     END-START.
***  売上ファイル存在チェック
     MOVE     SPACE          TO   SONZAI-CHK.
*#2017/11/16 NAV ST
     MOVE     SPACE          TO   CHK-FLG.
*#2017/11/16 NAV ED
     PERFORM COMRHEF-READ-SEC UNTIL  CHK-FLG = "CHK".
***  伝票纏め対象データが存在しない場合
*↓20171121NG修正
*    IF  SONZAI-CHK  = SPACE
*        MOVE   11  TO   ERR-FLG
*        MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTDATE
*        MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
*        MOVE  "R"  TO   EDIT-OPTION OF DSP-BTDATE
*        MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTORI
*        GO         TO   PARA-CHK-EXIT
*    ELSE
*        MOVE SPACE TO   EDIT-CURSOR OF DSP-BTDATE
*        MOVE  "M"  TO   EDIT-OPTION OF DSP-BTTIME
*        MOVE  "M"  TO   EDIT-OPTION OF DSP-BTDATE
*        MOVE  "M"  TO   EDIT-OPTION OF DSP-BTTORI
*    END-IF.
*↑20171121NG修正
*
*↓20171121NG修正
     IF  SONZAI-CHK  = SPACE
         MOVE   6   TO   ERR-FLG
         MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTDATE
         MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
         MOVE  "R"  TO   EDIT-OPTION OF DSP-BTDATE
         MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTORI
         GO         TO   PARA-CHK-EXIT
     END-IF.
*
     IF  SONZAI-CHK  = "NOT"
         MOVE   11  TO   ERR-FLG
         MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTDATE
         MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
         MOVE  "R"  TO   EDIT-OPTION OF DSP-BTDATE
         MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTORI
         GO         TO   PARA-CHK-EXIT
     END-IF.
*
     IF  SONZAI-CHK  = "CHK"
         MOVE SPACE TO   EDIT-CURSOR OF DSP-BTDATE
         MOVE  "M"  TO   EDIT-OPTION OF DSP-BTTIME
         MOVE  "M"  TO   EDIT-OPTION OF DSP-BTDATE
         MOVE  "M"  TO   EDIT-OPTION OF DSP-BTTORI
     END-IF.
*↑20171121NG修正
*
 PARA-CHK-EXIT.
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
                PERFORM PARA-SET-SEC
                MOVE    "END"        TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     8       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*　　ﾊﾟﾗﾒﾀｾｯﾄ
****************************************************************
 PARA-SET-SEC            SECTION.
*
     MOVE   "PARA-SET-SEC"      TO   S-NAME.
*
     MOVE      DSP-BTDATE         TO    PARA-HIDUKE.
     MOVE      DSP-BTTIME         TO    PARA-JIKAN.
     MOVE      DSP-BTTORI         TO    PARA-TORCD.
     MOVE      DSP-HAKKBN         TO    PARA-HAKKBN.
     MOVE      DSP-SYUKBN         TO    PARA-SYUKBN.
     DISPLAY "DATE   = " DSP-BTDATE UPON CONS.
     DISPLAY "TIME   = " DSP-BTTIME UPON CONS.
     DISPLAY "TOKCD  = " DSP-BTTORI UPON CONS.
     DISPLAY "HAKKBN = " DSP-HAKKBN UPON CONS.
     DISPLAY "SYUKBN = " DSP-SYUKBN UPON CONS.
*
 PARA-SET-EXIT.
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
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJR03601"          TO   DSP-FMT.
     WRITE    DSP-FJR03601.
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
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FJR03601"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FJR03601.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*ＰＧＩＤ
     MOVE    "SJR0360I"           TO   DSP-PGID.
*ＦＯＲＭＩＤ
     MOVE    "FJR03601"           TO   DSP-FORMID.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
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
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTDATE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTDATE.
***  バッチ_（時間）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTIME.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTIME.
***  バッチ_（取引先）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BTTORI.
***  発行区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKKBN.
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUKBN.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE             HTOKMS COMRHEF DSPFILE.
**
 END-EXIT.
     EXIT.
****************************************************************
*  （共通）取引先マスタ読込　　　                              *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ  HTOKMS
           INVALID     MOVE  "INV"   TO   HTOKMS-INV-FLG
           NOT INVALID MOVE  SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*  （共通）当日スケジュールマスタ読込                          *
****************************************************************
 COMRHEF-READ-SEC      SECTION.
*
     MOVE     "COMRHEF-READ-SEC"  TO   S-NAME.
*
     READ  COMRHEF
           AT  END     MOVE  "CHK"   TO   CHK-FLG
           GO                        TO   COMRHEF-READ-EXIT
     END-READ.
*
     IF  DSP-BTDATE =    RMH-F78 AND
         DSP-BTTIME =    RMH-F79 AND
         DSP-BTTORI =    RMH-F01
         CONTINUE
     ELSE
         MOVE    "CHK"               TO   CHK-FLG
         GO                          TO   COMRHEF-READ-EXIT
     END-IF.
*
     IF  RMH-F27  =  "1"
*↓20171113NG修正
         MOVE    "NOT"               TO   SONZAI-CHK
*↑20171113NG修正
         GO                          TO   COMRHEF-READ-SEC
     ELSE
         MOVE    "CHK"               TO   SONZAI-CHK
     END-IF.
*
 COMRHEF-READ-EXIT.
     EXIT.
*****************<<  SJR0360I   END PROGRAM  >>******************
