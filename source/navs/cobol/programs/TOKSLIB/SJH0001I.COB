****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　ＥＯＳ管理マスタ保守　　　　　　　*
*    作成日／更新日　　　：　99/08/31                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJH0001I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/08/31.
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
*ＥＯＳ管理マスタ
     SELECT  JHMEOSF   ASSIGN    TO        DA-01-VI-JHMEOSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       EOS-F01
                                           EOS-F02
                                           EOS-F03
                       FILE      STATUS    EOS-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
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
*    FILE = ＥＯＳ管理マスタ                                   *
****************************************************************
 FD  JHMEOSF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMEOSF   OF   XFDLIB
                       JOINING   EOS       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJH00011  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  EOS-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
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
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
*
     03  IN-DATA                  PIC  X(01)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
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
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_終了　_取消".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_終了　_取消　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい番号を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"該当データが存在しません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"ＥＯＳ管理マスタに登録済です".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"ＥＯＳ管理マスタに未登録です".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"Ｉ又はＴを入力して下さい".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"ＡＡ又はＢＢを入力して下さい".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"電送レコード長エラー".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"データを入力して下さい".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS   9  PIC   N(20).
*
 01  FILE-ERR.
     03  EOS-ERR           PIC N(15) VALUE
                        NC"ＥＯＳ管理マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
                        NC"画面ファイルエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 EOS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMEOSF.
     DISPLAY     EOS-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     EOS-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       JHMEOSF.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTOKMS.
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
*処理区分入力
         WHEN      "1"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "2"  PERFORM   DSP-HEAD2-SEC
*明細項目入力
         WHEN      "3"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "4"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-HEAD1-SEC
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF       DSP-SHORI      =    ZERO
              MOVE      1         TO   ERR-FLG
              GO                  TO   HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF    DSP-SHORI    =   1   OR   2   OR   3
              MOVE     "2"        TO   PSW
*             同一モードでループさせるため
              MOVE     DSP-SHORI  TO   SAV-SHORI
     ELSE
              MOVE     1          TO   ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD2-SEC         SECTION.
     MOVE     "DSP-HEAD2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-HEAD2-SEC
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE     "HEAD2-CHK-SEC"     TO   S-NAME.
*キー項目 未入力チェック
***  ＳＥＱ－ＮＯ
     IF       DSP-SEQNO      =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SEQNO
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SEQNO
              GO             TO   HEAD2-CHK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SEQNO
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SEQNO
     END-IF.
*
***  取引先コード
     IF       DSP-TORICD  =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-TORICD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-TORICD
              GO             TO   HEAD2-CHK-EXIT
     ELSE
***       取引先コード存在チェック（取引先マスタ ＲＥＡＤ）
              PERFORM   HTOKMS-READ
              IF        READ-FLG    =   ZERO
***           存在すればＯＫ（取引先名→画面）
                    MOVE    TOK-F03        TO   DSP-TORINM
                    MOVE  "M"     TO  EDIT-OPTION  OF  DSP-TORICD
                    MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-TORICD
              ELSE
***           存在しなければＮＧ（空白→画面）
                    MOVE    SPACE          TO   DSP-TORINM
                    MOVE     2             TO   ERR-FLG
                    MOVE  "R"   TO   EDIT-OPTION   OF  DSP-TORICD
                    MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-TORICD
                    GO                     TO   HEAD2-CHK-EXIT
              END-IF
     END-IF.
*
***  受配信区分
     IF       DSP-JHCD    =   1   OR   2
              IF    DSP-JHCD       =    1
                    MOVE     NC"受信"       TO   DSP-JHNM
              ELSE
                    MOVE     NC"配信"       TO   DSP-JHNM
              END-IF
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-JHCD
              MOVE   SPACE   TO   EDIT-CURSOR  OF  DSP-JHCD
     ELSE
              IF     DSP-JHCD  =  SPACE
                     MOVE    8    TO   ERR-FLG
              ELSE
                     MOVE    1    TO   ERR-FLG
              END-IF
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-JHCD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-JHCD
              GO             TO   HEAD2-CHK-EXIT
     END-IF.
*
*ＥＯＳ管理マスタ ＲＥＡＤ
     PERFORM   JHMEOSF-READ.
***  処理区分＝１（登録）
     IF       DSP-SHORI   =   1
***           データが存在すればエラー
              IF    READ-FLG   =   ZERO
                    MOVE     3            TO   ERR-FLG
              ELSE
***           データが存在しなければＯＫ（ボディ入力へ）
                    MOVE    "3"           TO   PSW
              END-IF
     ELSE
***  処理区分＝２（修正），３（削除）
***           データが存在すればＯＫ
              IF    READ-FLG   =   ZERO
                    PERFORM   MOVE-DSP-SEC
***                 処理区分＝２（修正）は，ボディ入力へ
                    IF    DSP-SHORI  =  2
                          MOVE    "3"          TO   PSW
                    ELSE
***                 処理区分＝３（削除）は，確認入力へ
                          MOVE    "4"          TO   PSW
                    END-IF
              ELSE
***        データが存在しなければエラー
                    MOVE     4            TO   ERR-FLG
              END-IF
     END-IF.
*
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 3 )              2.3       *
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
*               MOVE    "4"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     9       TO   ERR-FLG
                GO       TO      DSP-BODY-SEC
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             明細項目チェック                       2.3.1     *
****************************************************************
 BODY-CHK-SEC          SECTION.
     MOVE     "BODY-CHK-SEC"      TO   S-NAME.
*
*回線種別
     IF       DSP-KAICD   =  "I" OR  "T"
              MOVE  "M"      TO   EDIT-OPTION    OF  DSP-KAICD
              MOVE   SPACE   TO   EDIT-CURSOR    OF  DSP-KAICD
              IF       DSP-KAICD  = "I"
                       MOVE  NC"ＩＳＤＮ"   TO  DSP-KAINM
              ELSE
                       MOVE  NC"公衆回線"   TO  DSP-KAINM
              END-IF
*             MOVE     "SCREEN"             TO  DSP-GRP
*             PERFORM  DSP-WRITE-SEC
     ELSE
              MOVE   5       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION    OF   DSP-KAICD
              MOVE  "C"      TO   EDIT-CURSOR    OF   DSP-KAICD
              GO             TO   BODY-CHK-EXIT
*             MOVE  "SCREEN" TO   DSP-GRP
*             PERFORM  DSP-WRITE-SEC
     END-IF.
*
*制御ＩＤ
     IF       DSP-SEIID   =  "AA" OR  "BB"
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-SEIID
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-SEIID
     ELSE
              MOVE   6    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-SEIID
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-SEIID
              GO          TO   BODY-CHK-EXIT
     END-IF.
*
*センターコード
     IF       DSP-CTCD  =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION    OF  DSP-CTCD
              MOVE  "C"   TO   EDIT-CURSOR    OF  DSP-CTCD
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-CTCD
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-CTCD
     END-IF.
*センター識別子
     IF       DSP-CTSIKI  =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-CTSIKI
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-CTSIKI
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-CTSIKI
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-CTSIKI
     END-IF.
*センター取引先
     IF       DSP-CTTORI  =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-CTTORI
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-CTTORI
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-CTTORI
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-CTTORI
     END-IF.
*識別子
     IF       DSP-SIKI    =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-SIKI
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-SIKI
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-SIKI
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-SIKI
     END-IF.
*電話番号
     IF       DSP-TELNO   =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-TELNO
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-TELNO
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-TELNO
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-TELNO
     END-IF.
*電送レコード長
     IF       DSP-DENREC  =   0128  OR  0256  OR  2048
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-DENREC
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-DENREC
     ELSE
              MOVE   7    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-DENREC
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-DENREC
              GO          TO   BODY-CHK-EXIT
     END-IF.
*データ編集ＩＤ
     IF       DSP-DATAID  =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-DATAID
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-DATAID
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-DATAID
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-DATAID
     END-IF.
*ＣＶＣＳ管理Ｆ名
     IF       DSP-CVCSNM  =  SPACE
              MOVE   8    TO   ERR-FLG
              MOVE  "R"   TO   EDIT-OPTION   OF  DSP-CVCSNM
              MOVE  "C"   TO   EDIT-CURSOR   OF  DSP-CVCSNM
              GO          TO   BODY-CHK-EXIT
     ELSE
              MOVE  "M"     TO  EDIT-OPTION  OF  DSP-CVCSNM
              MOVE   SPACE  TO  EDIT-CURSOR  OF  DSP-CVCSNM
     END-IF.
*
     MOVE     "4"       TO   PSW.
*
 BODY-CHK-EXIT.
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
                EVALUATE  DSP-SHORI
*-------------------登録
                    WHEN  "1"
                          PERFORM     FILE-WRT-SEC
*-------------------修正
                    WHEN  "2"
                          PERFORM     FILE-UPD-SEC
*-------------------削除
                    WHEN  "3"
                          PERFORM     FILE-DLT-SEC
                END-EVALUATE
                PERFORM   INIT-DSP-SEC
                MOVE    "2"      TO   PSW
                MOVE   SAV-SHORI TO   DSP-SHORI
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-SHORI   =  1   OR   2
                    MOVE    "3"       TO   PSW
                ELSE
                    MOVE    "1"       TO   PSW
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
*        ＥＯＳ管理マスタ更新　処理区分＝１（登録）  2.4.1     *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   EOS-REC.
     INITIALIZE                        EOS-REC.
*キー項目転送
***  ＳＥＱ_
     MOVE     DSP-SEQNO           TO   EOS-F01.
***  取引先コード
     MOVE     DSP-TORICD          TO   EOS-F03.
***  受配信区分コード
     MOVE     DSP-JHCD            TO   EOS-F02.
*明細項目
     PERFORM  MOVE-JHMEOSF.
*登録日付
*    MOVE     SYS-DATE            TO   HAIS-F98.
*ＥＯＳ管理マスタ登録
     WRITE    EOS-REC.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*        ＥＯＳ管理マスタ更新　処理区分＝２（修正）  2.4.2     *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
*キー項目転送
***  ＳＥＱ_
     MOVE     DSP-SEQNO           TO   EOS-F01.
***  取引先コード
     MOVE     DSP-TORICD          TO   EOS-F03.
***  受配信区分コード
     MOVE     DSP-JHCD            TO   EOS-F02.
*ＥＯＳ管理マスタ ＲＥＡＤ（存在すればレコード更新）
     PERFORM   JHMEOSF-READ.
     IF    READ-FLG   =   ZERO
           PERFORM   MOVE-JHMEOSF
           REWRITE   EOS-REC
     ELSE
           DISPLAY
           NC"未登録です　ＳＥＱ－ＮＯ＝"  DSP-SEQNO UPON CONS
           DISPLAY
           NC"　　　　　　取引先コード＝"  DSP-TORICD UPON CONS
           DISPLAY
           NC"　　　　　　回線種別＝"  DSP-KAICD  UPON CONS
     END-IF.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*        ＥＯＳ管理マスタ更新　処理区分＝３（削除）  2.4.3     *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
*キー項目転送
***  ＳＥＱ_
     MOVE     DSP-SEQNO           TO   EOS-F01.
***  取引先コード
     MOVE     DSP-TORICD          TO   EOS-F03.
***  受配信区分コード
     MOVE     DSP-JHCD            TO   EOS-F02.
*ＥＯＳ管理マスタ ＲＥＡＤ（存在すればレコード削除）
     PERFORM   JHMEOSF-READ.
     IF    READ-FLG   =   ZERO
           DELETE    JHMEOSF
     ELSE
           DISPLAY   NC"未登録です　ＫＥＹ＝　"  DSP-SEQNO
                     UPON CONS
     END-IF.
*
 FILE-DLT-EXIT.
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
*処理区分
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
*キー項目
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
*明細項目
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
*確認
         WHEN   "4"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FJH00011"          TO   DSP-FMT.
     WRITE    DSP-FJH00011.
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
*処理区分
         WHEN   "1"
                MOVE    "SMODE"   TO   DSP-GRP
*キー項目
         WHEN   "2"
                MOVE    "KEYGRP"  TO   DSP-GRP
*明細項目
         WHEN   "3"
                MOVE    "MAIN"    TO   DSP-GRP
*確認
         WHEN   "4"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJH00011"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             項目転送  （ＥＯＳ管理マスタ → 画面）
****************************************************************
 MOVE-DSP-SEC           SECTION.
     MOVE     "MOVE-DSP-SEC"      TO   S-NAME.
*回線種別
     MOVE    EOS-F04              TO   DSP-KAICD.
     IF      EOS-F04  = "I"
             MOVE     NC"ＩＳＤＮ"     TO   DSP-KAINM
     ELSE
             MOVE     NC"公衆回線"     TO   DSP-KAINM
     END-IF.
*制御ＩＤ
     MOVE    EOS-F05              TO   DSP-SEIID.
*センターコード
     MOVE    EOS-F06              TO   DSP-CTCD.
*センター識別子
     MOVE    EOS-F07              TO   DSP-CTSIKI.
*センター取引先
     MOVE    EOS-F08              TO   DSP-CTTORI.
*識別子
     MOVE    EOS-F09              TO   DSP-SIKI.
*電話番号
     MOVE    EOS-F10              TO   DSP-TELNO.
*電送レコード長
     MOVE    EOS-F11              TO   DSP-DENREC.
*データ編集ＩＤ
     MOVE    EOS-F12              TO   DSP-DATAID.
*ＣＶＣＳ管理Ｆ名
     MOVE    EOS-F13              TO   DSP-CVCSNM.
*
 MOVE-DSP-EXIT.
     EXIT.
****************************************************************
*             項目転送  （画面 → ＥＯＳ管理マスタ）
****************************************************************
 MOVE-JHMEOSF        SECTION.
*回線種別
     MOVE    DSP-KAICD            TO   EOS-F04.
*制御ＩＤ
     MOVE    DSP-SEIID            TO   EOS-F05.
*センターコード
     MOVE    DSP-CTCD             TO   EOS-F06.
*センター識別子
     MOVE    DSP-CTSIKI           TO   EOS-F07.
*センター取引先
     MOVE    DSP-CTTORI           TO   EOS-F08.
*識別子
     MOVE    DSP-SIKI             TO   EOS-F09.
*電話番号
     MOVE    DSP-TELNO            TO   EOS-F10.
*電送レコード長
     MOVE    DSP-DENREC           TO   EOS-F11.
*データ編集ＩＤ
     MOVE    DSP-DATAID           TO   EOS-F12.
*ＣＶＣＳ管理Ｆ名
     MOVE    DSP-CVCSNM           TO   EOS-F13.
*
 MOVE-JHMEOSF-EXIT.
     EXIT.
****************************************************************
*               取引先マスタ　ＲＥＡＤ                         *
****************************************************************
 HTOKMS-READ             SECTION.
     MOVE     "HTOKMS-READ"       TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
*取引先ＲＥＡＤ（該当データ無，READ-FLG=1）
     MOVE    DSP-TORICD           TO   TOK-F01.
     READ    HTOKMS
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             ＥＯＳ管理マスタ　ＲＥＡＤ                       *
****************************************************************
 JHMEOSF-READ             SECTION.
     MOVE     "JHMEOSF-READ"      TO   S-NAME.
*
     MOVE    ZERO                 TO   READ-FLG.
*ＥＯＳ管理マスタＲＥＡＤ（該当データ無，READ-FLG=1）
     MOVE    DSP-SEQNO            TO   EOS-F01.
     MOVE    DSP-TORICD           TO   EOS-F03.
     MOVE    DSP-JHCD             TO   EOS-F02.
     READ    JHMEOSF
         INVALID
             MOVE    1            TO   READ-FLG
     END-READ.
*
 JHMEOSF-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FJH00011.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*リバース，カーソルパーク解除
***  ＳＥＱ－ＮＯ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SEQNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SEQNO.
***  取引先コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TORICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TORICD.
***  受配信区分
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-JHCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-JHCD.
***  回線種別
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KAICD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KAICD.
***  制御ＩＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SEIID.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SEIID.
***  センターコード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CTCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CTCD.
***  センター識別子
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CTSIKI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CTSIKI.
***  センター取引先
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CTTORI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CTTORI.
***  識別子
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SIKI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SIKI.
***  電話番号
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TELNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TELNO.
***  電送レコード長
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DENREC.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DENREC.
***  データ編集ＩＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DATAID.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DATAID.
***  ＣＶＣＳ管理Ｆ名
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-CVCSNM.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-CVCSNM.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             DSPFILE
                       JHMEOSF   HTOKMS.
**
 END-EXIT.
     EXIT.
*****************<<  SJH0001I   END PROGRAM  >>******************
