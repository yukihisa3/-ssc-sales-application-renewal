# SSY5170I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY5170I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　オンライン業務　　　　　　        *
*    モジュール名　　　　：　発注確認　データ作成指示　　　　　*
*    作成日／作成者　　　：　2019/02/20 INOUE                  *
*    処理概要　　　　　　：　ＥＸＣＥＬ引渡条件指定　　　　　　*
*    　　　　　　　　　　：　　　　　　                        *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　                    *
*    　　　　　　　　　　：　　　　　　                        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY5170I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2019/02/20.
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
*作場マスタ
     SELECT  SAKUBAF   ASSIGN    TO        SAKUBAL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SAK-F01
                       FILE      STATUS    SAK-ST.
*条件ファイル
*    SELECT  JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
*                      ORGANIZATION        INDEXED
*                      ACCESS    MODE      RANDOM
*                      RECORD    KEY       JYO-F01
*                                          JYO-F02
*                      FILE      STATUS    JYO-ST.
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
*  FILE= 作場マスタ                                          *
****************************************************************
 FD  SAKUBAF
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SAKUBAF   OF   XFDLIB
                       JOINING   SAK       AS   PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
*FD  JYOKEN1           LABEL RECORD   IS   STANDARD.
*                      COPY      JYOKEN1   OF   XFDLIB
*                      JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSY51701  OF   XMDLIB
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
*    03  JYO-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*画面表示用ワーク
 01  DSP-WORK.
     03  WORK-PGID                PIC  X(08)  VALUE  "SSY5170I".
     03  WORK-FORMID              PIC  X(08)  VALUE  "FSY51701".
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
     03  SAKUBAF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WK-SAKU.
     03  WK-SAKUCD                PIC  X(02)  OCCURS 20.
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
             VALUE NC"管理番号を入力して下さい".
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
*    03  ERR-MSG11.
*        05  FILLER              PIC   N(20)
*            VALUE NC"商品分類ＣＤが登録されていません".
*    03  ERR-MSG12.
*        05  FILLER              PIC   N(20)
*            VALUE NC"正しい発行区分を指定してください".
*    03  ERR-MSG13.
*        05  FILLER              PIC   N(20)
*            VALUE NC"正しい出力区分を指定してください".
*    03  ERR-MSG14.
*        05  FILLER              PIC   N(20)
*            VALUE NC"納品日（日付）論理エラー".
*    03  ERR-MSG15.
*        05  FILLER              PIC   N(20)
*            VALUE NC"正しい集約区分を指定してください".
*
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  10  PIC   N(20).
*    03  ERR-MSG-R   OCCURS  13  PIC   N(20).
*    03  ERR-MSG-R   OCCURS  15  PIC   N(20).
*
 01  FILE-ERR.
     03  DEN-ERR           PIC N(20) VALUE
                        NC"売上伝票ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  SAK-ERR           PIC N(20) VALUE
                        NC"作場マスタエラー".
*    03  JYO-ERR           PIC N(20) VALUE
*                       NC"条件ファイルエラー".
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
 01  LINK-KANRINO        PIC  9(08).
 01  LINK-SAKUBA.
     03  LINK-SAKU-01    PIC  X(02).
     03  LINK-SAKU-02    PIC  X(02).
     03  LINK-SAKU-03    PIC  X(02).
     03  LINK-SAKU-04    PIC  X(02).
     03  LINK-SAKU-05    PIC  X(02).
     03  LINK-SAKU-06    PIC  X(02).
     03  LINK-SAKU-07    PIC  X(02).
     03  LINK-SAKU-08    PIC  X(02).
     03  LINK-SAKU-09    PIC  X(02).
     03  LINK-SAKU-10    PIC  X(02).
     03  LINK-SAKU-11    PIC  X(02).
     03  LINK-SAKU-12    PIC  X(02).
     03  LINK-SAKU-13    PIC  X(02).
     03  LINK-SAKU-14    PIC  X(02).
     03  LINK-SAKU-15    PIC  X(02).
     03  LINK-SAKU-16    PIC  X(02).
     03  LINK-SAKU-17    PIC  X(02).
     03  LINK-SAKU-18    PIC  X(02).
     03  LINK-SAKU-19    PIC  X(02).
     03  LINK-SAKU-20    PIC  X(02).
*
**************************************************************
 PROCEDURE       DIVISION  USING     LINK-KANRINO
                                     LINK-SAKUBA.
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
 SAK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SAKUBAF.
     DISPLAY     SAK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SAK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*JYO-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE JYOKEN1.
*    DISPLAY     JYO-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     JYO-ST    UPON      CONS.
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
     OPEN     INPUT     SAKUBAF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
     INITIALIZE         LINK-KANRINO
                        LINK-SAKU-01
                        LINK-SAKU-02
                        LINK-SAKU-03
                        LINK-SAKU-04
                        LINK-SAKU-05
                        LINK-SAKU-06
                        LINK-SAKU-07
                        LINK-SAKU-08
                        LINK-SAKU-09
                        LINK-SAKU-10
                        LINK-SAKU-11
                        LINK-SAKU-12
                        LINK-SAKU-13
                        LINK-SAKU-14
                        LINK-SAKU-15
                        LINK-SAKU-16
                        LINK-SAKU-17
                        LINK-SAKU-18
                        LINK-SAKU-19
                        LINK-SAKU-20.
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
                MOVE    "1"      TO   PSW
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
 PARA-CHK-01.
     IF       PSW   =  "2"
              GO             TO   PARA-CHK-02
     END-IF.
*
*管理番号チェック
***  未入力チェック
     IF       DSP-KANNO  NOT NUMERIC
         OR   DSP-KANNO  =  ZERO
              MOVE   1       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-KANNO
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-KANNO
     ELSE
              MOVE   0       TO   ERR-FLG
              MOVE  "M"      TO   EDIT-OPTION  OF  DSP-KANNO
              MOVE  " "      TO   EDIT-CURSOR  OF  DSP-KANNO
              MOVE  "2"      TO   PSW
     END-IF.
     GO                      TO   PARA-CHK-EXIT.
*
 PARA-CHK-02.
*作場コードチェック
     MOVE     ZERO           TO       ERR-FLG.
     IF       DSP-SKB001     = SPACE  AND
              DSP-SKB002     = SPACE  AND
              DSP-SKB003     = SPACE  AND
              DSP-SKB004     = SPACE  AND
              DSP-SKB005     = SPACE  AND
              DSP-SKB006     = SPACE  AND
              DSP-SKB007     = SPACE  AND
              DSP-SKB008     = SPACE  AND
              DSP-SKB009     = SPACE  AND
              DSP-SKB010     = SPACE  AND
              DSP-SKB011     = SPACE  AND
              DSP-SKB012     = SPACE  AND
              DSP-SKB013     = SPACE  AND
              DSP-SKB014     = SPACE  AND
              DSP-SKB015     = SPACE  AND
              DSP-SKB016     = SPACE  AND
              DSP-SKB017     = SPACE  AND
              DSP-SKB018     = SPACE  AND
              DSP-SKB019     = SPACE  AND
              DSP-SKB020     = SPACE
              MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB001
              MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB001
              MOVE   6   TO  ERR-FLG
              GO         TO  PARA-CHK-EXIT
     END-IF.
*1
     IF       DSP-SKB001 NOT = SPACE
              MOVE       DSP-SKB001 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB001
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB001
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB001
                   END-IF
              ELSE
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB001
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB001
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB001
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB001
     END-IF.
*2
     IF       DSP-SKB002 NOT = SPACE
              MOVE       DSP-SKB002 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB002
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB002
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB002
                   END-IF
              ELSE
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB002
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB002
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB002
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB002
     END-IF.
*3
     IF       DSP-SKB003 NOT = SPACE
              MOVE       DSP-SKB003 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB003
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB003
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB003
                   END-IF
              ELSE
                         MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB003
                         MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB003
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB003
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB003
     END-IF.
*4
     IF       DSP-SKB004 NOT = SPACE
              MOVE       DSP-SKB004 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB004
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB004
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB004
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB004
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB004
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB004
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB004
     END-IF.
*5
     IF       DSP-SKB005 NOT = SPACE
              MOVE       DSP-SKB005 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB005
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB005
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB005
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB005
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB005
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB005
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB005
     END-IF.
*6
     IF       DSP-SKB006 NOT = SPACE
              MOVE       DSP-SKB006 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB006
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB006
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB006
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB006
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB006
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB006
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB006
     END-IF.
*7
     IF       DSP-SKB007 NOT = SPACE
              MOVE       DSP-SKB007 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB007
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB007
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB007
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB007
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB007
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB007
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB007
     END-IF.
*8
     IF       DSP-SKB008 NOT = SPACE
              MOVE       DSP-SKB008 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB008
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB008
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB008
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB008
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB008
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB008
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB008
     END-IF.
*9
     IF       DSP-SKB009 NOT = SPACE
              MOVE       DSP-SKB009 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB009
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB009
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB009
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB009
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB009
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB009
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB009
     END-IF.
*10
     IF       DSP-SKB010 NOT = SPACE
              MOVE       DSP-SKB010 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB010
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB010
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB010
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB010
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB010
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB010
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB010
     END-IF.
*11
     IF       DSP-SKB011 NOT = SPACE
              MOVE       DSP-SKB011 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB011
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB011
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB011
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB011
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB011
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB011
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB011
     END-IF.
*12
     IF       DSP-SKB012 NOT = SPACE
              MOVE       DSP-SKB012 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB012
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB012
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB012
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB012
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB012
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB012
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB012
     END-IF.
*13
     IF       DSP-SKB013 NOT = SPACE
              MOVE       DSP-SKB013 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB013
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB013
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB013
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB013
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB013
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB013
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB013
     END-IF.
*14
     IF       DSP-SKB014 NOT = SPACE
              MOVE       DSP-SKB014 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB014
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB014
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB014
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB014
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB014
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB014
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB014
     END-IF.
*15
     IF       DSP-SKB015 NOT = SPACE
              MOVE       DSP-SKB015 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB015
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB015
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB015
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB015
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB015
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB015
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB015
     END-IF.
*16
     IF       DSP-SKB016 NOT = SPACE
              MOVE       DSP-SKB016 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB016
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB016
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB016
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB016
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB016
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB016
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB016
     END-IF.
*17
     IF       DSP-SKB017 NOT = SPACE
              MOVE       DSP-SKB017 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB017
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB017
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB017
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB017
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB017
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB017
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB017
     END-IF.
*18
     IF       DSP-SKB018 NOT = SPACE
              MOVE       DSP-SKB018 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB018
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB018
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB018
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB018
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB018
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB018
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB018
     END-IF.
*19
     IF       DSP-SKB019 NOT = SPACE
              MOVE       DSP-SKB019 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB019
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB019
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB019
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB019
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB019
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB019
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB019
     END-IF.
*20
     IF       DSP-SKB020 NOT = SPACE
              MOVE       DSP-SKB020 TO  SAK-F01
              PERFORM    SAKUBAF-READ-SEC
              IF         SAKUBAF-INV-FLG = "INV"
                   IF    ERR-FLG     = ZERO
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB020
                         MOVE  "C"  TO  EDIT-CURSOR OF DSP-SKB020
                         MOVE   7   TO  ERR-FLG
                   ELSE
                         MOVE  "R"  TO  EDIT-OPTION OF DSP-SKB020
                   END-IF
              ELSE
                   MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB020
                   MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB020
              END-IF
     ELSE
              MOVE  "M"  TO  EDIT-OPTION OF DSP-SKB020
              MOVE  " "  TO  EDIT-CURSOR OF DSP-SKB020
     END-IF.
*
     IF       ERR-FLG     = ZERO
              MOVE    "3"       TO    PSW
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             作場マスタ索引                                   *
****************************************************************
 SAKUBAF-READ-SEC     SECTION.
     MOVE     "SAKUBAF-READ-SEC" TO   S-NAME.
*
     READ      SAKUBAF
        INVALID
               MOVE    "INV"     TO   SAKUBAF-INV-FLG
        NOT INVALID
               MOVE    "   "     TO   SAKUBAF-INV-FLG
     END-READ.
*
 SAKUBAF-READ-EXIT.
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
                MOVE    DSP-KANNO     TO   LINK-KANRINO
                MOVE    SPACE         TO   WK-SAKU
                MOVE    1             TO   IX
                IF      DSP-SKB001 NOT =   SPACE
                        MOVE  DSP-SKB001   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                END-IF
                IF      DSP-SKB002 NOT =   SPACE
                    IF  DSP-SKB002 NOT =   DSP-SKB001
                        MOVE  DSP-SKB002   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB003 NOT =   SPACE
                    IF  DSP-SKB003 NOT =   DSP-SKB001 AND
                        DSP-SKB003 NOT =   DSP-SKB002
                        MOVE  DSP-SKB003   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB004 NOT =   SPACE
                    IF  DSP-SKB004 NOT =   DSP-SKB001 AND
                        DSP-SKB004 NOT =   DSP-SKB002 AND
                        DSP-SKB004 NOT =   DSP-SKB003
                        MOVE  DSP-SKB004   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB005 NOT =   SPACE
                    IF  DSP-SKB005 NOT =   DSP-SKB001 AND
                        DSP-SKB005 NOT =   DSP-SKB002 AND
                        DSP-SKB005 NOT =   DSP-SKB003 AND
                        DSP-SKB005 NOT =   DSP-SKB004
                        MOVE  DSP-SKB005   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB006 NOT =   SPACE
                    IF  DSP-SKB006 NOT =   DSP-SKB001 AND
                        DSP-SKB006 NOT =   DSP-SKB002 AND
                        DSP-SKB006 NOT =   DSP-SKB003 AND
                        DSP-SKB006 NOT =   DSP-SKB004 AND
                        DSP-SKB006 NOT =   DSP-SKB005
                        MOVE  DSP-SKB006   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB007 NOT =   SPACE
                    IF  DSP-SKB007 NOT =   DSP-SKB001 AND
                        DSP-SKB007 NOT =   DSP-SKB002 AND
                        DSP-SKB007 NOT =   DSP-SKB003 AND
                        DSP-SKB007 NOT =   DSP-SKB004 AND
                        DSP-SKB007 NOT =   DSP-SKB005 AND
                        DSP-SKB007 NOT =   DSP-SKB006
                        MOVE  DSP-SKB007   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB008 NOT =   SPACE
                    IF  DSP-SKB008 NOT =   DSP-SKB001 AND
                        DSP-SKB008 NOT =   DSP-SKB002 AND
                        DSP-SKB008 NOT =   DSP-SKB003 AND
                        DSP-SKB008 NOT =   DSP-SKB004 AND
                        DSP-SKB008 NOT =   DSP-SKB005 AND
                        DSP-SKB008 NOT =   DSP-SKB006 AND
                        DSP-SKB008 NOT =   DSP-SKB007
                        MOVE  DSP-SKB008   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB009 NOT =   SPACE
                    IF  DSP-SKB009 NOT =   DSP-SKB001 AND
                        DSP-SKB009 NOT =   DSP-SKB002 AND
                        DSP-SKB009 NOT =   DSP-SKB003 AND
                        DSP-SKB009 NOT =   DSP-SKB004 AND
                        DSP-SKB009 NOT =   DSP-SKB005 AND
                        DSP-SKB009 NOT =   DSP-SKB006 AND
                        DSP-SKB009 NOT =   DSP-SKB007 AND
                        DSP-SKB009 NOT =   DSP-SKB008
                        MOVE  DSP-SKB009   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB010 NOT =   SPACE
                    IF  DSP-SKB010 NOT =   DSP-SKB001 AND
                        DSP-SKB010 NOT =   DSP-SKB002 AND
                        DSP-SKB010 NOT =   DSP-SKB003 AND
                        DSP-SKB010 NOT =   DSP-SKB004 AND
                        DSP-SKB010 NOT =   DSP-SKB005 AND
                        DSP-SKB010 NOT =   DSP-SKB006 AND
                        DSP-SKB010 NOT =   DSP-SKB007 AND
                        DSP-SKB010 NOT =   DSP-SKB008 AND
                        DSP-SKB010 NOT =   DSP-SKB009
                        MOVE  DSP-SKB010   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB011 NOT =   SPACE
                    IF  DSP-SKB011 NOT =   DSP-SKB001 AND
                        DSP-SKB011 NOT =   DSP-SKB002 AND
                        DSP-SKB011 NOT =   DSP-SKB003 AND
                        DSP-SKB011 NOT =   DSP-SKB004 AND
                        DSP-SKB011 NOT =   DSP-SKB005 AND
                        DSP-SKB011 NOT =   DSP-SKB006 AND
                        DSP-SKB011 NOT =   DSP-SKB007 AND
                        DSP-SKB011 NOT =   DSP-SKB008 AND
                        DSP-SKB011 NOT =   DSP-SKB009 AND
                        DSP-SKB011 NOT =   DSP-SKB010
                        MOVE  DSP-SKB011   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB012 NOT =   SPACE
                    IF  DSP-SKB012 NOT =   DSP-SKB001 AND
                        DSP-SKB012 NOT =   DSP-SKB002 AND
                        DSP-SKB012 NOT =   DSP-SKB003 AND
                        DSP-SKB012 NOT =   DSP-SKB004 AND
                        DSP-SKB012 NOT =   DSP-SKB005 AND
                        DSP-SKB012 NOT =   DSP-SKB006 AND
                        DSP-SKB012 NOT =   DSP-SKB007 AND
                        DSP-SKB012 NOT =   DSP-SKB008 AND
                        DSP-SKB012 NOT =   DSP-SKB009 AND
                        DSP-SKB012 NOT =   DSP-SKB010 AND
                        DSP-SKB012 NOT =   DSP-SKB011
                        MOVE  DSP-SKB012   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB013 NOT =   SPACE
                    IF  DSP-SKB013 NOT =   DSP-SKB001 AND
                        DSP-SKB013 NOT =   DSP-SKB002 AND
                        DSP-SKB013 NOT =   DSP-SKB003 AND
                        DSP-SKB013 NOT =   DSP-SKB004 AND
                        DSP-SKB013 NOT =   DSP-SKB005 AND
                        DSP-SKB013 NOT =   DSP-SKB006 AND
                        DSP-SKB013 NOT =   DSP-SKB007 AND
                        DSP-SKB013 NOT =   DSP-SKB008 AND
                        DSP-SKB013 NOT =   DSP-SKB009 AND
                        DSP-SKB013 NOT =   DSP-SKB010 AND
                        DSP-SKB013 NOT =   DSP-SKB011 AND
                        DSP-SKB013 NOT =   DSP-SKB012
                        MOVE  DSP-SKB013   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB014 NOT =   SPACE
                    IF  DSP-SKB014 NOT =   DSP-SKB001 AND
                        DSP-SKB014 NOT =   DSP-SKB002 AND
                        DSP-SKB014 NOT =   DSP-SKB003 AND
                        DSP-SKB014 NOT =   DSP-SKB004 AND
                        DSP-SKB014 NOT =   DSP-SKB005 AND
                        DSP-SKB014 NOT =   DSP-SKB006 AND
                        DSP-SKB014 NOT =   DSP-SKB007 AND
                        DSP-SKB014 NOT =   DSP-SKB008 AND
                        DSP-SKB014 NOT =   DSP-SKB009 AND
                        DSP-SKB014 NOT =   DSP-SKB010 AND
                        DSP-SKB014 NOT =   DSP-SKB011 AND
                        DSP-SKB014 NOT =   DSP-SKB012 AND
                        DSP-SKB014 NOT =   DSP-SKB013
                        MOVE  DSP-SKB014   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB015 NOT =   SPACE
                    IF  DSP-SKB015 NOT =   DSP-SKB001 AND
                        DSP-SKB015 NOT =   DSP-SKB002 AND
                        DSP-SKB015 NOT =   DSP-SKB003 AND
                        DSP-SKB015 NOT =   DSP-SKB004 AND
                        DSP-SKB015 NOT =   DSP-SKB005 AND
                        DSP-SKB015 NOT =   DSP-SKB006 AND
                        DSP-SKB015 NOT =   DSP-SKB007 AND
                        DSP-SKB015 NOT =   DSP-SKB008 AND
                        DSP-SKB015 NOT =   DSP-SKB009 AND
                        DSP-SKB015 NOT =   DSP-SKB010 AND
                        DSP-SKB015 NOT =   DSP-SKB011 AND
                        DSP-SKB015 NOT =   DSP-SKB012 AND
                        DSP-SKB015 NOT =   DSP-SKB013 AND
                        DSP-SKB015 NOT =   DSP-SKB014
                        MOVE  DSP-SKB015   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB016 NOT =   SPACE
                    IF  DSP-SKB016 NOT =   DSP-SKB001 AND
                        DSP-SKB016 NOT =   DSP-SKB002 AND
                        DSP-SKB016 NOT =   DSP-SKB003 AND
                        DSP-SKB016 NOT =   DSP-SKB004 AND
                        DSP-SKB016 NOT =   DSP-SKB005 AND
                        DSP-SKB016 NOT =   DSP-SKB006 AND
                        DSP-SKB016 NOT =   DSP-SKB007 AND
                        DSP-SKB016 NOT =   DSP-SKB008 AND
                        DSP-SKB016 NOT =   DSP-SKB009 AND
                        DSP-SKB016 NOT =   DSP-SKB010 AND
                        DSP-SKB016 NOT =   DSP-SKB011 AND
                        DSP-SKB016 NOT =   DSP-SKB012 AND
                        DSP-SKB016 NOT =   DSP-SKB013 AND
                        DSP-SKB016 NOT =   DSP-SKB014 AND
                        DSP-SKB016 NOT =   DSP-SKB015
                        MOVE  DSP-SKB016   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB017 NOT =   SPACE
                    IF  DSP-SKB017 NOT =   DSP-SKB001 AND
                        DSP-SKB017 NOT =   DSP-SKB002 AND
                        DSP-SKB017 NOT =   DSP-SKB003 AND
                        DSP-SKB017 NOT =   DSP-SKB004 AND
                        DSP-SKB017 NOT =   DSP-SKB005 AND
                        DSP-SKB017 NOT =   DSP-SKB006 AND
                        DSP-SKB017 NOT =   DSP-SKB007 AND
                        DSP-SKB017 NOT =   DSP-SKB008 AND
                        DSP-SKB017 NOT =   DSP-SKB009 AND
                        DSP-SKB017 NOT =   DSP-SKB010 AND
                        DSP-SKB017 NOT =   DSP-SKB011 AND
                        DSP-SKB017 NOT =   DSP-SKB012 AND
                        DSP-SKB017 NOT =   DSP-SKB013 AND
                        DSP-SKB017 NOT =   DSP-SKB014 AND
                        DSP-SKB017 NOT =   DSP-SKB015 AND
                        DSP-SKB017 NOT =   DSP-SKB016
                        MOVE  DSP-SKB017   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB018 NOT =   SPACE
                    IF  DSP-SKB018 NOT =   DSP-SKB001 AND
                        DSP-SKB018 NOT =   DSP-SKB002 AND
                        DSP-SKB018 NOT =   DSP-SKB003 AND
                        DSP-SKB018 NOT =   DSP-SKB004 AND
                        DSP-SKB018 NOT =   DSP-SKB005 AND
                        DSP-SKB018 NOT =   DSP-SKB006 AND
                        DSP-SKB018 NOT =   DSP-SKB007 AND
                        DSP-SKB018 NOT =   DSP-SKB008 AND
                        DSP-SKB018 NOT =   DSP-SKB009 AND
                        DSP-SKB018 NOT =   DSP-SKB010 AND
                        DSP-SKB018 NOT =   DSP-SKB011 AND
                        DSP-SKB018 NOT =   DSP-SKB012 AND
                        DSP-SKB018 NOT =   DSP-SKB013 AND
                        DSP-SKB018 NOT =   DSP-SKB014 AND
                        DSP-SKB018 NOT =   DSP-SKB015 AND
                        DSP-SKB018 NOT =   DSP-SKB016 AND
                        DSP-SKB018 NOT =   DSP-SKB017
                        MOVE  DSP-SKB018   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB019 NOT =   SPACE
                    IF  DSP-SKB019 NOT =   DSP-SKB001 AND
                        DSP-SKB019 NOT =   DSP-SKB002 AND
                        DSP-SKB019 NOT =   DSP-SKB003 AND
                        DSP-SKB019 NOT =   DSP-SKB004 AND
                        DSP-SKB019 NOT =   DSP-SKB005 AND
                        DSP-SKB019 NOT =   DSP-SKB006 AND
                        DSP-SKB019 NOT =   DSP-SKB007 AND
                        DSP-SKB019 NOT =   DSP-SKB008 AND
                        DSP-SKB019 NOT =   DSP-SKB009 AND
                        DSP-SKB019 NOT =   DSP-SKB010 AND
                        DSP-SKB019 NOT =   DSP-SKB011 AND
                        DSP-SKB019 NOT =   DSP-SKB012 AND
                        DSP-SKB019 NOT =   DSP-SKB013 AND
                        DSP-SKB019 NOT =   DSP-SKB014 AND
                        DSP-SKB019 NOT =   DSP-SKB015 AND
                        DSP-SKB019 NOT =   DSP-SKB016 AND
                        DSP-SKB019 NOT =   DSP-SKB017 AND
                        DSP-SKB019 NOT =   DSP-SKB018
                        MOVE  DSP-SKB019   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                IF      DSP-SKB020 NOT =   SPACE
                    IF  DSP-SKB020 NOT =   DSP-SKB001 AND
                        DSP-SKB020 NOT =   DSP-SKB002 AND
                        DSP-SKB020 NOT =   DSP-SKB003 AND
                        DSP-SKB020 NOT =   DSP-SKB004 AND
                        DSP-SKB020 NOT =   DSP-SKB005 AND
                        DSP-SKB020 NOT =   DSP-SKB006 AND
                        DSP-SKB020 NOT =   DSP-SKB007 AND
                        DSP-SKB020 NOT =   DSP-SKB008 AND
                        DSP-SKB020 NOT =   DSP-SKB009 AND
                        DSP-SKB020 NOT =   DSP-SKB010 AND
                        DSP-SKB020 NOT =   DSP-SKB011 AND
                        DSP-SKB020 NOT =   DSP-SKB012 AND
                        DSP-SKB020 NOT =   DSP-SKB013 AND
                        DSP-SKB020 NOT =   DSP-SKB014 AND
                        DSP-SKB020 NOT =   DSP-SKB015 AND
                        DSP-SKB020 NOT =   DSP-SKB016 AND
                        DSP-SKB020 NOT =   DSP-SKB017 AND
                        DSP-SKB020 NOT =   DSP-SKB018 AND
                        DSP-SKB020 NOT =   DSP-SKB019
                        MOVE  DSP-SKB020   TO   WK-SAKUCD(IX)
                        ADD   1            TO   IX
                    END-IF
                END-IF
                MOVE    WK-SAKU            TO   LINK-SAKUBA
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
     MOVE    "FSY51701"          TO   DSP-FMT.
     WRITE    DSP-FSY51701.
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
                MOVE    "GRP001"  TO   DSP-GRP
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "ENDCHK"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FSY51701"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FSY51701.
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
***  管理番号
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-KANNO.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-KANNO.
***  作場コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKB001
                          EDIT-OPTION  OF  DSP-SKB002
                          EDIT-OPTION  OF  DSP-SKB003
                          EDIT-OPTION  OF  DSP-SKB004
                          EDIT-OPTION  OF  DSP-SKB005
                          EDIT-OPTION  OF  DSP-SKB006
                          EDIT-OPTION  OF  DSP-SKB007
                          EDIT-OPTION  OF  DSP-SKB008
                          EDIT-OPTION  OF  DSP-SKB009
                          EDIT-OPTION  OF  DSP-SKB010
                          EDIT-OPTION  OF  DSP-SKB011
                          EDIT-OPTION  OF  DSP-SKB012
                          EDIT-OPTION  OF  DSP-SKB013
                          EDIT-OPTION  OF  DSP-SKB014
                          EDIT-OPTION  OF  DSP-SKB015
                          EDIT-OPTION  OF  DSP-SKB016
                          EDIT-OPTION  OF  DSP-SKB017
                          EDIT-OPTION  OF  DSP-SKB018
                          EDIT-OPTION  OF  DSP-SKB019
                          EDIT-OPTION  OF  DSP-SKB020.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKB001
                          EDIT-CURSOR  OF  DSP-SKB002
                          EDIT-CURSOR  OF  DSP-SKB003
                          EDIT-CURSOR  OF  DSP-SKB004
                          EDIT-CURSOR  OF  DSP-SKB005
                          EDIT-CURSOR  OF  DSP-SKB006
                          EDIT-CURSOR  OF  DSP-SKB007
                          EDIT-CURSOR  OF  DSP-SKB008
                          EDIT-CURSOR  OF  DSP-SKB009
                          EDIT-CURSOR  OF  DSP-SKB010
                          EDIT-CURSOR  OF  DSP-SKB011
                          EDIT-CURSOR  OF  DSP-SKB012
                          EDIT-CURSOR  OF  DSP-SKB013
                          EDIT-CURSOR  OF  DSP-SKB014
                          EDIT-CURSOR  OF  DSP-SKB015
                          EDIT-CURSOR  OF  DSP-SKB016
                          EDIT-CURSOR  OF  DSP-SKB017
                          EDIT-CURSOR  OF  DSP-SKB018
                          EDIT-CURSOR  OF  DSP-SKB019
                          EDIT-CURSOR  OF  DSP-SKB020.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SAKUBAF  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SSY5170I   END PROGRAM  >>******************

```
