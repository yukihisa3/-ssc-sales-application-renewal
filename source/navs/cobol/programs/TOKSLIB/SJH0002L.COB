****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信                            *
*    業務名　　　　　　　：　ＥＯＳ管理                        *
*    モジュール名　　　　：　ＥＯＳ管理マスタリスト            *
*    作成日／更新日　　　：　99/08/26                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH0002L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/08/26.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
           YA      IS   CHR-2
           YB-21   IS   CHR-21
           YB      IS   CHR-15
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＯＳ管理マスタ
     SELECT     JHMEOSF    ASSIGN    TO        DA-01-VI-JHMEOSL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       JHM-F01
                                               JHM-F02
                                               JHM-F03
                           FILE      STATUS    JHM-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*プリント定義ファイル
     SELECT     PRTFILE       ASSIGN    TO        LP-04-PRTF
                           FILE      STATUS    PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
*  FILE=ＥＯＳ管理マスタ                                       *
****************************************************************
 FD  JHMEOSF
*    BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMEOSF   OF        XFDLIB
     JOINING     JHM       AS        PREFIX.
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*
***  ｽﾃｰﾀｽｴﾘｱ
 01  FILE-STATUS.
     03  JHM-ST            PIC X(02).
     03  TOK-ST            PIC X(02).
     03  PRT-ST            PIC X(02).
***  ﾌﾗｸﾞｴﾘｱ
 01  FLG-AREA.
     03  END-FLG           PIC X(03).
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
     03  SYSDATE           PIC 9(06) VALUE     ZERO.
     03  RD-SW             PIC 9(01) VALUE     ZERO.
*
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
 01  FILE-ERR.
     03  JHM-ERR           PIC N(15) VALUE
                        NC"ＥＯＳ管理マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンターエラー".
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(17)  VALUE
         NC"※※　ＥＯＳ管理マスタリスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(04)  VALUE  "ﾃﾞｰﾀ".
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"区分"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"受配信区分"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"取引先"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(27)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"使用回線"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  X(02)  VALUE  "ID".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(08)  VALUE  "ｾﾝﾀｰｺｰﾄﾞ".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(06)  VALUE  "ﾄﾘﾋｷｻｷ".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  X(06)  VALUE  "ｼｷﾍﾞﾂｼ".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(04)  VALUE  "ﾃﾞｰﾀ".
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"種"
                                 CHARACTER   TYPE   IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(03)  VALUE  "TEL".
     03  FILLER                  PIC  X(15)  VALUE  SPACE.
     03  FILLER                  PIC  X(05)  VALUE  "ﾚｺｰﾄﾞ".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(07)  VALUE  "ﾍﾝｼｭｳID".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  X(06)  VALUE  "ﾃﾞｰﾀID".
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-00                  PIC  X(02).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-01                  PIC  N(02).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  DT1-02                  PIC  N(02).
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-03                  PIC  ZZZZZZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-04                  PIC  N(15).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-05                  PIC  X(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-06                  PIC  N(04).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-07                  PIC  X(02).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-08                  PIC  X(06).
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-09                  PIC  X(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-10                  PIC  X(06).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-11                  PIC  X(02).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  DT1-12                  PIC  X(17).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-13                  PIC  ZZZ9.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-14                  PIC  X(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-15                  PIC  X(08).
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 JHM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMEOSF.
     DISPLAY     JHM-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JHM-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 PROCESS-START             SECTION.
     MOVE        "PROCESS-START"     TO          S-NAME.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG = "END"
     PERFORM     END-SEC.
     STOP        RUN.
 PROC-EXIT.
     EXIT.
****************************************************************
*             初期処理                                1.0      *
****************************************************************
 INIT-SEC                  SECTION.
     MOVE        "INIT-SEC"          TO          S-NAME.
*システム日付・時刻の取得
     ACCEPT   SYSDATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSDATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-DATE8.
*ファイルＯＰＥＮ
     OPEN        INPUT     JHMEOSF
                           HTOKMS
                 OUTPUT    PRTFILE.
*初期値セット
     MOVE        ZERO      TO        P-CNT.
     MOVE        SPACE     TO        END-FLG.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC                  SECTION.
     MOVE        "MAIN-SEC"          TO          S-NAME.
*改頁チェック
     IF   L-CNT  >  60
*         改頁
          MOVE          SPACE     TO        PRT-REC
          WRITE   PRT-REC  AFTER  PAGE
*
          PERFORM  HEAD-WT-SEC
     END-IF.
*
     PERFORM     FL-READ-SEC.
*
     IF   END-FLG  NOT  =  "END"
          PERFORM       BODY-WT-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                    2.1      *
****************************************************************
 FL-READ-SEC            SECTION.
     MOVE     "FL-READ-SEC"       TO             S-NAME.
*
     READ        JHMEOSF   AT        END
         MOVE    "END"     TO        END-FLG
         GO                TO        FL-READ-EXIT
     END-READ.
*
     IF       RD-SW     =    0
              PERFORM   HEAD-WT-SEC
              MOVE      1         TO   RD-SW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.

****************************************************************
*             明細出力処理                            2.2      *
****************************************************************
 BODY-WT-SEC                  SECTION.
     MOVE        "BODY-WT-SEC"      TO          S-NAME.
     MOVE        SPACE               TO     DT1.
*
     IF       WK-TORICD  =  JHM-F03
              GO            TO       DT-010
     ELSE
          MOVE   JHM-F03   TO        DT1-03  WK-TORICD
     END-IF.
*取引先ＲＥＡＤ
     MOVE        JHM-F03   TO        TOK-F01.
     READ     HTOKMS
     INVALID
          MOVE   SPACE     TO        DT1-04
     NOT INVALID
          MOVE   TOK-F03   TO        DT1-04
     END-READ.
*
 DT-010.
***  データ区分
     EVALUATE      JHM-F01
         WHEN      "01"
                   MOVE      JHM-F01        TO   DT1-00
                   MOVE      NC"発注"       TO   DT1-01
         WHEN      "02"
                   MOVE      JHM-F01        TO   DT1-00
                   MOVE      NC"支払"       TO   DT1-01
         WHEN      "03"
                   MOVE      JHM-F01        TO   DT1-00
                   MOVE      NC"請求"       TO   DT1-01
         WHEN      "04"
                   MOVE      JHM-F01        TO   DT1-00
                   MOVE      NC"受領"       TO   DT1-01
         WHEN      "05"
                   MOVE      JHM-F01        TO   DT1-00
                   MOVE      NC"商品"       TO   DT1-01
     END-EVALUATE.
***  受配信区分
     IF       JHM-F02   =    1
              MOVE    NC"受信"         TO        DT1-02
     ELSE
              MOVE    NC"配信"         TO        DT1-02
     END-IF.
***  使用回線
     IF       JHM-F04   =    "I"
              MOVE      JHM-F04        TO        DT1-05
              MOVE    NC"ＩＳＤＮ"     TO        DT1-06
     ELSE
              MOVE      JHM-F04        TO        DT1-05
              MOVE    NC"公衆回線"     TO        DT1-06
     END-IF.
***  制御ＩＤ
     MOVE          JHM-F05   TO        DT1-07.
***  センターコード
     MOVE          JHM-F06   TO        DT1-08.
***  ＣＶＣＳ取引先
     MOVE          JHM-F08   TO        DT1-09.
***  識別子
     MOVE          JHM-F09   TO        DT1-10.
***  データ種
     MOVE          JHM-F10   TO        DT1-11.
***  電話番号
     MOVE          JHM-F11   TO        DT1-12.
***  電送レコード長
     MOVE          JHM-F12   TO        DT1-13.
***  データ編集ＰＧＩＤ
     MOVE          JHM-F13   TO        DT1-14.
***  伝票データＩＤ
     MOVE          JHM-F14   TO        DT1-15.
*
*明細部出力
     WRITE    PRT-REC   FROM      DT1       AFTER  1.
     WRITE    PRT-REC   FROM      SEN1      AFTER  1.
*
     ADD      2         TO        L-CNT.
 BODY-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3      *
****************************************************************
 HEAD-WT-SEC                  SECTION.
     MOVE        "HEAD-WT-SEC"      TO          S-NAME.
*ページカウント
     ADD         1         TO        P-CNT.
*項目設定
***  プログラムＩＤ
     MOVE     "SJH0002L"          TO        HD1-01.
***  日付
     MOVE     WK-YYYY             TO        HD1-02.
     MOVE     WK-MM               TO        HD1-03.
     MOVE     WK-DD               TO        HD1-04.
***  ページ_
     MOVE     P-CNT               TO        HD1-05.
*
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     MOVE     ZERO         TO        WK-TORICD.
     MOVE     ZERO         TO        L-CNT.
     ADD         5         TO        L-CNT.
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                                3.0      *
****************************************************************
 END-SEC                   SECTION.
     MOVE        "END-SEC"           TO          S-NAME.
     CLOSE       PRTFILE    JHMEOSF    HTOKMS.
 END-EXIT.
     EXIT.
