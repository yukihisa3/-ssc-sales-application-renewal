****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　オンライン業務　　　　　　        *
*    モジュール名　　　　：　オンライン作場振分情報リスト　　　*
*    　　　　　　　　　　：　（出力指示）　　　　　　　　　　　*
*    作成日／作成者　　　：　2015/03/30 INOUE                  *
*    処理概要　　　　　　：　リスト発行指示　　　　　　　　　　*
*    　　　　　　　　　　：　　　　　　                        *
*    更新日／更新者　　　：　2016/08/22 INOUE                  *
*    修正概要　　　　　　：　本発ＥＤＩ対応                    *

*    更新日／更新者　　　：　2016/09/15 TAKAHASHI              *
*    修正概要　　　　　　：　本発ＥＤＩ対応（納品日、集約対応）*
*                          　                                  *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3813I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2015/03/30.
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
     SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
                                           DEN-F03
                       FILE      STATUS    DEN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SAK-F01
                       FILE      STATUS    SAK-ST.
*↓2016/08/22
*条件ファイル
     SELECT  JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    JYO-ST.
*↑2016/08/22
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
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   DEN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 作場マスタ                                          *
****************************************************************
 FD  SAKUBAF
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SAK       AS   PREFIX.
*↓2016/08/22
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1           LABEL RECORD   IS   STANDARD.
                       COPY      JYOKEN1   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
*↑2016/08/22
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY38131  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  SAK-ST                   PIC  X(02).
*↓2016/08/22
     03  JYO-ST                   PIC  X(02).
*↑2016/08/22
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY3813I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY38131".
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
             VALUE NC"作場コードを入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"作場マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"開始が終了を超えています".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"売上伝票ファイルに登録されていません".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出力順を指定してください".
*↓2016/08/22
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"商品分類ＣＤが登録されていません".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい発行区分を指定してください".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい出力区分を指定してください".
*↑2016/08/22
*↓2016/09/15
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"納品日（日付）論理エラー".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい集約区分を指定してください".
*↑2016/09/15
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
*↓2016/08/22
*    03  ERR-MSG-R   OCCURS  10  PIC   N(20).
*↓2016/09/15
*****03  ERR-MSG-R   OCCURS  13  PIC   N(20).
     03  ERR-MSG-R   OCCURS  15  PIC   N(20).
*↑2016/09/15
*↑2016/08/22
*
 01  FILE-ERR.
     03  DEN-ERR           PIC N(20) VALUE
                        NC"売上伝票ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  SAK-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
*↓2016/08/22
     03  JYO-ERR           PIC N(20) VALUE
                        NC"条件ファイルエラー".
*↑2016/08/22
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
 01  LINK-SAKU-KAISI     PIC  X(02).
 01  LINK-SAKU-OWARI     PIC  X(02).
 01  LINK-JUN            PIC  X(01).
*↓2016/08/22
 01  LINK-SYOBUN-KAISI   PIC  X(08).
 01  LINK-SYOBUN-OWARI   PIC  X(08).
 01  LINK-PATN-KAISI     PIC  X(02).
 01  LINK-PATN-OWARI     PIC  X(02).
 01  LINK-HAKKOU         PIC  X(01).
 01  LINK-OUTPUT         PIC  X(01).
*↑2016/08/22
*↓2016/09/15
 01  LINK-SYKKBN         PIC  X(01).
 01  LINK-NOUHDT         PIC  9(08).
*↑2016/09/15
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-DATE
                                     LINK-TIME
                                     LINK-TORICD
                                     LINK-SAKU-KAISI
                                     LINK-SAKU-OWARI
                                     LINK-JUN
*↓                                 2016/08/22
                                     LINK-SYOBUN-KAISI
                                     LINK-SYOBUN-OWARI
                                     LINK-PATN-KAISI
                                     LINK-PATN-OWARI
                                     LINK-HAKKOU
*↓                                 2016/09/15
*************************************LINK-OUTPUT.
                                     LINK-OUTPUT
*↑                                 2016/08/22
                                     LINK-SYKKBN
                                     LINK-NOUHDT.
*↑                                 2016/09/15
**************************************************************
 DECLARATIVES.
 DEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SAK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SAK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SAK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↓2016/08/22
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYOKEN1.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↑2016/08/22
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
     OPEN     INPUT     SHTDENF   HTOKMS   SAKUBAF.
*↓2016/08/22
     OPEN     INPUT     JYOKEN1.
*↑2016/08/22
*ワークの初期化
     INITIALIZE         FLG-AREA.
     INITIALIZE   LINK-DATE LINK-TIME LINK-TORICD
                  LINK-SAKU-KAISI     LINK-SAKU-OWARI LINK-JUN.
*↓2016/08/22
     INITIALIZE   LINK-SYOBUN-KAISI   LINK-SYOBUN-OWARI
                  LINK-PATN-KAISI     LINK-PATN-OWARI
                  LINK-HAKKOU         LINK-OUTPUT.
*↑2016/08/22
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
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
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
              MOVE     DSP-BTYMD     TO   LINK-IN-YMD8
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
*開始作場コードチェック
***  未入力時，開始作場コード＝０
     IF       DSP-SKBCDS   =   SPACE
              MOVE     SPACE      TO   DSP-SKBCDS
     ELSE
         IF   DSP-SKBCDS   =   "00"
              CONTINUE
         ELSE
***  作場マスタＲＥＡＤ
              MOVE      DSP-SKBCDS     TO   SAK-F01
              READ      SAKUBAF
              INVALID
                     MOVE   7     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCDS
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCDS
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SKBCDS
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SKBCDS
              END-READ
         END-IF
     END-IF.
*
*終了作場コードチェック
***  未入力時，終了作場コード＝９９
     IF       DSP-SKBCDE   =   SPACE
     OR       DSP-SKBCDE   =   "00"
     OR       DSP-SKBCDE   =   "99"
              MOVE        "99"    TO   DSP-SKBCDE
     ELSE
***  作場マスタＲＥＡＤ
              MOVE      DSP-SKBCDE     TO   SAK-F01
              READ      SAKUBAF
              INVALID
                     MOVE   7     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SKBCDE
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SKBCDE
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SKBCDE
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SKBCDE
              END-READ
     END-IF.
*
*作場コード大小チェック
     IF       DSP-SKBCDS  >  DSP-SKBCDE
              MOVE      8    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SKBCDS
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SKBCDE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SKBCDS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SKBCDS
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SKBCDE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SKBCDS
     END-IF.
*
*売上伝票ファイル存在チェック
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     DSP-BTYMD     TO   DEN-F46.
     MOVE     DSP-BTTIME     TO   DEN-F47.
     MOVE     DSP-BTTOKC     TO   DEN-F01.
     MOVE     DSP-SKBCDS     TO   DEN-F48.
*****DISPLAY "DSP-SKBCDS = " DSP-SKBCDS UPON CONS.
*****DISPLAY "DEN-F48    = " DEN-F48    UPON CONS.
     START    SHTDENF   KEY  IS   >=   DEN-F46  DEN-F47  DEN-F01
                                       DEN-F48  DEN-F02  DEN-F04
                                       DEN-F051 DEN-F07  DEN-F112
                                       DEN-F03
     INVALID
              MOVE      9    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTYMD
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTIME
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-BTTOKC
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-BTYMD
              GO             TO   PARA-CHK-EXIT
     NOT INVALID
***           売上ファイル存在チェック
              READ     SHTDENF
              END-READ
***           開始作場コードがゼロの時，バッチ_のみチェック
              IF   DSP-SKBCDS  =  SPACE
                   IF  DSP-BTYMD =    DEN-F46 AND
                       DSP-BTTIME =    DEN-F47 AND
                       DSP-BTTOKC =    DEN-F01
                       MOVE  SPACE TO  EDIT-CURSOR OF DSP-BTYMD
                   ELSE
                       MOVE   9   TO   ERR-FLG
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-BTYMD
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTOKC
                       MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTYMD
                       GO         TO   PARA-CHK-EXIT
                   END-IF
***           ゼロ以外，開始作場コードまでチェック
              ELSE
*           DISPLAY "DSP-SKBCDS = " DSP-SKBCDS UPON CONS
*           DISPLAY "DSP-SKBCDE = " DSP-SKBCDE UPON CONS
*           DISPLAY "DEN-F48    = " DEN-F48    UPON CONS
                   IF  DSP-BTYMD =    DEN-F46 AND
                       DSP-BTTIME =    DEN-F47 AND
                       DSP-BTTOKC =    DEN-F01 AND
                     ( DSP-SKBCDS <=   DEN-F48 AND
                       DSP-SKBCDE >=   DEN-F48 )
                       MOVE  SPACE TO  EDIT-CURSOR OF DSP-BTYMD
                   ELSE
                       MOVE   9   TO   ERR-FLG
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-BTYMD
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTIME
                       MOVE  "R"  TO   EDIT-OPTION OF DSP-BTTOKC
                       MOVE  "C"  TO   EDIT-CURSOR OF DSP-BTYMD
                       GO         TO   PARA-CHK-EXIT
                   END-IF
              END-IF
     END-START.
*↓2016/09/15
***  納品日未入力はゼロをセット
     IF       DSP-NOUHDT  NOT NUMERIC
         OR   DSP-NOUHDT  =  ZERO
              MOVE   ZERO    TO   DSP-NOUHDT
     ELSE
***           納品日（日付）論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-NOUHDT     TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  14       TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUHDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUHDT
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUHDT
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUHDT
     END-IF.
*↑2016/09/15
*
*出力順チェック
     IF    (  DSP-SYUKBN NOT =    "1"  )  AND
           (  DSP-SYUKBN NOT =    "2"  )
              MOVE     10    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SYUKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SYUKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SYUKBN
     END-IF.
*
*↓2016/08/22
*開始商品分類コードチェック
     IF       DSP-SHKBNS   =   SPACE
              CONTINUE
     ELSE
***  条件ファイルチェック
              MOVE      "05"           TO   JYO-F01
              MOVE      DSP-SHKBNS     TO   JYO-F02
              READ      JYOKEN1
              INVALID
                     MOVE  11     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SHKBNS
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SHKBNS
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SHKBNS
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SHKBNS
              END-READ
     END-IF.
*
*終了商品分類コードチェック
     IF       DSP-SHKBNE   =   SPACE
*    OR       DSP-SHKBNE   =   "99999999"
*             MOVE  "99999999"    TO   DSP-SHKBNE
              CONTINUE
     ELSE
***  条件ファイルチェック
              MOVE      "05"           TO   JYO-F01
              MOVE      DSP-SHKBNE     TO   JYO-F02
              READ      JYOKEN1
              INVALID
                     MOVE  11     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SHKBNE
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SHKBNE
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SHKBNE
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SHKBNE
              END-READ
     END-IF.
*
*商品分類コード大小チェック
     IF       DSP-SHKBNS  >  DSP-SHKBNE
              MOVE      8    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SHKBNS
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SHKBNE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SHKBNS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SHKBNS
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SHKBNE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SHKBNS
     END-IF.
*
*パターンコード大小チェック
     IF       DSP-PTCDS   >  DSP-PTCDE
              MOVE      8    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-PTCDS
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-PTCDE
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-PTCDS
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-PTCDS
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-PTCDE
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-PTCDS
     END-IF.
*
*発行区分チェック
     IF    (  DSP-HAKKBN NOT =    " "  )  AND
           (  DSP-HAKKBN NOT =    "1"  )
              MOVE     12    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-HAKKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-HAKKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-HAKKBN
     END-IF.
*
*出力区分チェック
     IF    (  DSP-OUTKBN NOT =    " "  )  AND
           (  DSP-OUTKBN NOT =    "1"  )
              MOVE     13    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-OUTKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-OUTKBN
     END-IF.
*↑2016/08/22
*
*↓2016/09/15
*集約区分チェック
     IF    (  DSP-SYKKBN NOT =    " "  )  AND
           (  DSP-SYKKBN NOT =    "1"  )
              MOVE     13    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-OUTKBN
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-OUTKBN
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-OUTKBN
     END-IF.
*↑2016/08/22
*
     MOVE    "2"       TO    PSW.
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
                MOVE    DSP-BTYMD    TO   LINK-DATE
                MOVE    DSP-BTTIME    TO   LINK-TIME
                MOVE    DSP-BTTOKC    TO   LINK-TORICD
                MOVE    DSP-SKBCDS    TO   LINK-SAKU-KAISI
                MOVE    DSP-SKBCDE    TO   LINK-SAKU-OWARI
                MOVE    DSP-SYUKBN    TO   LINK-JUN
*↓2016/08/22
                MOVE    DSP-SHKBNS    TO   LINK-SYOBUN-KAISI
                MOVE    DSP-SHKBNE    TO   LINK-SYOBUN-OWARI
                MOVE    DSP-PTCDS     TO   LINK-PATN-KAISI
                MOVE    DSP-PTCDE     TO   LINK-PATN-OWARI
                MOVE    DSP-HAKKBN    TO   LINK-HAKKOU
                MOVE    DSP-OUTKBN    TO   LINK-OUTPUT
*↑2016/08/22
*↓2016/09/15
                MOVE    DSP-NOUHDT    TO   LINK-NOUHDT
                MOVE    DSP-SYKKBN    TO   LINK-SYKKBN
*↑2016/09/15
*        DISPLAY "LINK-SAKU-KAISI = " LINK-SAKU-KAISI UPON CONS
*        DISPLAY "LINK-SAKU-OWARI = " LINK-SAKU-OWARI UPON CONS
                MOVE    "END"         TO   END-FLG
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
     MOVE    "FSY38131"          TO   DSP-FMT.
     WRITE    DSP-FSY38131.
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
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY38131"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY38131.
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
***  開始作場コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBCDS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBCDS.
***  終了作場コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBCDE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBCDE.
***  出力順
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYUKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYUKBN.
*↓2016/08/22
***  開始商品分類コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SHKBNS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SHKBNS.
***  終了商品分類コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SHKBNE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SHKBNE.
***  開始パターンコード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-PTCDS.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-PTCDS.
***  終了パターンコード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-PTCDE.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-PTCDE.
***  発行区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HAKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HAKKBN.
***  出力区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-OUTKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-OUTKBN.
*↑2016/08/22
*↓2016/09/15
***  納品日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NOUHDT.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NOUHDT.
***  集約区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYKKBN.
*↑2016/09/15
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SHTDENF  HTOKMS  SAKUBAF  DSPFILE.
*↓2016/08/22
     CLOSE             JYOKEN1.
*↑2016/08/22
**
 END-EXIT.
     EXIT.
*****************<<  SSY3813I   END PROGRAM  >>******************
