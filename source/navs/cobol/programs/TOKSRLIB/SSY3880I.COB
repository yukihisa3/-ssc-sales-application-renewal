****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　作場変更、出荷数変更　　　        *
*    モジュール名　　　　：　ナフコ作場数量一括変更指示　　　　*
*    作成日／作成者　　　：　2018/11/27 NAV TAKAHASHI          *
*    処理概要　　　　　　：　ナフコオンラインデータの作場／　　*
*                          　数量を一括更新を行なう条件を入　　*
*                          　力し、後続バッチへパラメタ連携　　*
*                          　行なう。　　　　　　　　　　　　　*
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/11/27 高橋_　　新規作成
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3880I.
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
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*ナフコ店舗マスタ
     SELECT  NFTENMS   ASSIGN    TO        NFTENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F01
                                           TEN-F02
                       FILE      STATUS    TEN-ST.
*ナフコ商品マスタ
     SELECT  NFSHOMS   ASSIGN    TO        NFSHOMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SHO-F01
                       FILE      STATUS    SHO-ST.
*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SAK-F01
                       FILE      STATUS    SAK-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01  TAN-F02
                       FILE      STATUS    TAN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY38801  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = ナフコ店舗マスタ
****************************************************************
 FD  NFTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFTENMS   OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*    FILE = ナフコ商品マスタ
****************************************************************
 FD  NFSHOMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSHOMS   OF   XFDLIB
                       JOINING   SHO       AS   PREFIX.
****************************************************************
*    FILE= 作場マスタ
****************************************************************
 FD  SAKUBAF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SAK       AS   PREFIX.
****************************************************************
*    FILE= 取引先マスタ
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE= 担当者マスタ
****************************************************************
 FD  HTANMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTANMS    OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SHO-ST                   PIC  X(02).
     03  SAK-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TAN-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY3880I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY38801".
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
     03  WK-TIME6                 PIC  9(06)  VALUE  ZERO.
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
             VALUE NC"納品日（日付）論理エラー".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"ナフコ店舗マスタを入力して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(20)
             VALUE NC"ナフコ店舗マスタに登録されていません".
     03  ERR-MSG11.
         05  FILLER              PIC   N(20)
             VALUE NC"ナフコ商品マスタに登録されていません".
     03  ERR-MSG12.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい作場変更区分を指定してください".
     03  ERR-MSG13.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい数量変更区分を指定してください".
     03  ERR-MSG14.
         05  FILLER              PIC   N(20)
             VALUE NC"変更作場を入力して下さい".
     03  ERR-MSG15.
         05  FILLER              PIC   N(20)
             VALUE NC"変更数量を入力して下さい".
     03  ERR-MSG16.
         05  FILLER              PIC   N(20)
     VALUE NC"作場、数量変更区分何れかは指定して下さい".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  16  PIC   N(20).
*
 01  FILE-ERR.
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"ナフコ店舗マスタエラー".
     03  SHO-ERR           PIC N(20) VALUE
                        NC"ナフコ商品マスタエラー".
     03  SAK-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
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
 01  LINK-BUMON          PIC  X(04).
 01  LINK-TANCD          PIC  X(02).
 01  LINK-DATE           PIC  9(08).
 01  LINK-TIME           PIC  9(04).
 01  LINK-TORICD         PIC  X(08).
 01  LINK-SAKUBA         PIC  X(02).
 01  LINK-NOUDT          PIC  9(08).
 01  LINK-NFTENCD        PIC  9(05).
 01  LINK-NFSHOCD        PIC  X(08).
 01  LINK-SAKKBN         PIC  X(01).
 01  LINK-SURKBN         PIC  X(01).
 01  LINK-HEN-SAKUBA     PIC  X(02).
 01  LINK-HEN-SURYO      PIC  9(05).
 01  LINK-UPD-DATE       PIC  9(08).
 01  LINK-UPD-TIME       PIC  9(06).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-BUMON
                                     LINK-TANCD
                                     LINK-DATE
                                     LINK-TIME
                                     LINK-TORICD
                                     LINK-SAKUBA
                                     LINK-NOUDT
                                     LINK-NFTENCD
                                     LINK-NFSHOCD
                                     LINK-SAKKBN
                                     LINK-SURKBN
                                     LINK-HEN-SAKUBA
                                     LINK-HEN-SURYO
                                     LINK-UPD-DATE
                                     LINK-UPD-TIME.
**************************************************************
 DECLARATIVES.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSHOMS.
     DISPLAY     SHO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SHO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SAK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SAK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SAK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
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
     MOVE      LINK-OUT-YMD       TO   LINK-UPD-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:6)       TO   WK-TIME6.
     MOVE      WK-TIME(1:6)       TO   LINK-UPD-TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     NFTENMS NFSHOMS SAKUBAF HTOKMS HTANMS.
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
*範囲指定１
         WHEN      "1"  PERFORM   DSP-PARA1-SEC
*範囲指定２
         WHEN      "2"  PERFORM   DSP-PARA2-SEC
*確認入力
         WHEN      "3"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA1-SEC        SECTION.
     MOVE     "DSP-PARA1-SEC"    TO   S-NAME.
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
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA1-SEC
     END-EVALUATE.
*
 DSP-PARA1-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK1-SEC            SECTION.
     MOVE     "PARA-CHK1-SEC"    TO   S-NAME.
*
*バッチ_（日付）チェック
***  バッチ_（日付）未入力チェック
     IF       DSP-BTYMD  NOT NUMERIC
         OR   DSP-BTYMD  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-BTYMD
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-BTYMD
              GO             TO   PARA-CHK1-EXIT
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
                   GO             TO   PARA-CHK1-EXIT
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
                   GO             TO   PARA-CHK1-EXIT
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
                       GO          TO PARA-CHK1-EXIT
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
              GO             TO   PARA-CHK1-EXIT
     ELSE
***           取引先ＲＥＡＤ
              MOVE     DSP-BTTOKC TO   TOK-F01
              READ     HTOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BTTOKC
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-BTTOKC
                     MOVE   SPACE TO   DSP-BTTOKN
                     GO           TO   PARA-CHK1-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BTTOKC
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BTTOKC
                     MOVE  TOK-F03  TO DSP-BTTOKN
              END-READ
     END-IF.
*
*作場コードチェック
     IF       DSP-SAKBCD   =   SPACE
              MOVE   6     TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SAKBCD
              MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SAKBCD
              GO           TO   PARA-CHK1-EXIT
     ELSE
***  作場マスタＲＥＡＤ
              MOVE      DSP-SAKBCD     TO   SAK-F01
              READ      SAKUBAF
              INVALID
                     MOVE   7     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SAKBCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SAKBCD
                     MOVE  ALL NC"＊"  TO           DSP-SAKBNM
                     GO           TO   PARA-CHK1-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SAKBCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SAKBCD
                     MOVE  SAK-F02     TO           DSP-SAKBNM
              END-READ
     END-IF.
***  納品日
     IF       DSP-NOUHDT NOT NUMERIC
     OR       DSP-NOUHDT =  ZERO
              MOVE   ZERO    TO   DSP-NOUHDT
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUHDT
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUHDT
     ELSE
***           バッチ_（日付）論理チェック
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
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NOUHDT
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NOUHDT
                   GO             TO   PARA-CHK1-EXIT
              END-IF
*
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NOUHDT
              MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NOUHDT
     END-IF.
*
*ナフコ店舗ＣＤ
     IF       DSP-TENCD  NOT  NUMERIC
     OR       DSP-TENCD  =    ZERO
              MOVE   ZERO  TO   DSP-TENCD
              MOVE   NC"全店舗対象"          TO  DSP-TENNM
              MOVE  "M"    TO   EDIT-OPTION  OF  DSP-TENCD
              MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-TENCD
     ELSE
***  ナフコ店舗マスタ
              MOVE      DSP-BTTOKC     TO   TEN-F01
              MOVE      DSP-TENCD      TO   TEN-F02
              READ      NFTENMS
              INVALID
                     MOVE   10    TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TENCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TENCD
                     MOVE  ALL NC"＊"  TO           DSP-TENNM
                     GO           TO   PARA-CHK1-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-TENCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-TENCD
                     MOVE  TEN-F04     TO           DSP-TENNM
              END-READ
     END-IF.
*
*ナフコ商品ＣＤ
     IF       DSP-SYOCD   =   SPACE
              MOVE   SPACE TO   DSP-SYOCD
              MOVE  NC"全商品対象"           TO  DSP-SYONM
              MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SYOCD
              MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SYOCD
     ELSE
***  ナフコ商品マスタＲＥＡＤ
              MOVE      DSP-SYOCD      TO   SHO-F01
              READ      NFSHOMS
              INVALID
                     MOVE   11    TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SYOCD
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SYOCD
                     MOVE  ALL NC"＊"  TO           DSP-SYONM
                     GO           TO   PARA-CHK1-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SYOCD
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SYOCD
                     MOVE  SHO-F05     TO           DSP-SYONM
                     MOVE  SHO-F09     TO           DSP-IRISU
              END-READ
     END-IF.
*作場変更区分
     IF   DSP-SAKKBN  =  SPACE   OR  "1"
          MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SAKKBN
          MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SAKKBN
     ELSE
          MOVE   12    TO   ERR-FLG
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SAKKBN
          MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SAKKBN
          GO           TO   PARA-CHK1-EXIT
     END-IF.
*
*数量変更区分
     IF   DSP-SURKBN  =  SPACE   OR  "1"
          MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SURKBN
          MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SURKBN
     ELSE
          MOVE   13    TO   ERR-FLG
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SURKBN
          MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SURKBN
          GO           TO   PARA-CHK1-EXIT
     END-IF.
*
*作場／数量変更区分入力チェック
     IF   DSP-SAKKBN  =  SPACE
     AND  DSP-SURKBN  =  SPACE
          MOVE   16    TO   ERR-FLG
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SAKKBN
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SURKBN
          MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SAKKBN
          GO           TO   PARA-CHK1-EXIT
     ELSE
          MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SAKKBN
          MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SURKBN
          MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SAKKBN
     END-IF.
*
     MOVE       "2"    TO   PSW.
*
     MOVE       SPACE  TO   EDIT-STATUS OF DSP-HENSAK.
     MOVE       SPACE  TO   EDIT-STATUS OF DSP-HENSUR.
*
     IF   DSP-SAKKBN  =  "1"
          MOVE  SPACE  TO   EDIT-STATUS OF DSP-HENSAK
     ELSE
          MOVE  "X"    TO   EDIT-STATUS OF DSP-HENSAK
     END-IF.
*
     IF   DSP-SURKBN  =  "1"
          MOVE  SPACE  TO   EDIT-STATUS OF DSP-HENSUR
     ELSE
          MOVE  "X"    TO   EDIT-STATUS OF DSP-HENSUR
     END-IF.
*グループ２初期化
     MOVE       SPACE  TO   DSP-HENSAK  DSP-HENSNM.
     MOVE       ZERO   TO   DSP-HENSUR.
*
 PARA-CHK1-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 2 )                2.1       *
****************************************************************
 DSP-PARA2-SEC        SECTION.
     MOVE     "DSP-PARA2-SEC"    TO   S-NAME.
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
*取消
         WHEN   "F006"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA2-SEC
     END-EVALUATE.
*
 DSP-PARA2-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK2-SEC            SECTION.
     MOVE     "PARA-CHK2-SEC"    TO   S-NAME.
*
*変更作場コードチェック
     IF  DSP-SAKKBN  = "1"
       IF     DSP-HENSAK   =   SPACE
              MOVE   14    TO   ERR-FLG
              MOVE  "R"    TO   EDIT-OPTION  OF  DSP-HENSAK
              MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-HENSAK
              GO           TO   PARA-CHK2-EXIT
       ELSE
***  作場マスタＲＥＡＤ
              MOVE      DSP-HENSAK     TO   SAK-F01
              READ      SAKUBAF
              INVALID
                     MOVE   7     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-HENSAK
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-HENSAK
                     MOVE  ALL NC"＊"  TO           DSP-HENSNM
                     GO           TO   PARA-CHK2-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-HENSNM
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-HENSNM
                     MOVE  SAK-F02     TO           DSP-HENSNM
              END-READ
       END-IF
     END-IF.
*
     MOVE   "3"      TO    PSW.
*
 PARA-CHK2-EXIT.
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
                MOVE    DSP-SAKBCD    TO   LINK-SAKUBA
                MOVE    DSP-NOUHDT    TO   LINK-NOUDT
                MOVE    DSP-TENCD     TO   LINK-NFTENCD
                MOVE    DSP-SYOCD     TO   LINK-NFSHOCD
                MOVE    DSP-SAKKBN    TO   LINK-SAKKBN
                MOVE    DSP-SURKBN    TO   LINK-SURKBN
                MOVE    DSP-HENSAK    TO   LINK-HEN-SAKUBA
                MOVE    DSP-HENSUR    TO   LINK-HEN-SURYO
                MOVE    "END"         TO   END-FLG
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
*担当者マスタ索引
     MOVE      LINK-BUMON         TO   TAN-F01  DSP-BUMTAN(1:4).
     MOVE      LINK-TANCD         TO   TAN-F02  DSP-BUMTAN(6:2).
     MOVE      "-"                TO   DSP-BUMTAN(5:1).
     READ   HTANMS
            INVALID  MOVE ALL NC"＊"   TO  DSP-BUMTNM
        NOT INVALID  MOVE TAN-F03      TO  DSP-BUMTNM
     END-READ.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"   WHEN  "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*基準日付／時刻セット
     MOVE     LINK-UPD-DATE      TO   DSP-SKDATE.
     MOVE     LINK-UPD-TIME      TO   DSP-SKTIME.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSY38801"          TO   DSP-FMT.
     WRITE    DSP-FSY38801.
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
*条件１
         WHEN   "1"
                MOVE    "GRP001"  TO   DSP-GRP
*条件２
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY38801"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY38801.
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
***  作場コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SAKBCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SAKBCD.
***  ナフコ店舗コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TENCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TENCD.
***  ナフコ商品コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SYOCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SYOCD.
***  作場変更区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SAKKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SAKKBN.
***  数量変更区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SURKBN.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SURKBN.
***  変更作場ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HENSAK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HENSAK.
***  変更数量
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-HENSUR.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-HENSUR.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE              DSPFILE  NFTENMS   NFSHOMS  SAKUBAF
                        HTOKMS   HTANMS.
**
 END-EXIT.
     EXIT.
*****************<<  SSY3880I   END PROGRAM  >>******************
