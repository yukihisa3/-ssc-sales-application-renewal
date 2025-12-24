# SJH0017L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJH0017L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信                            *
*    業務名　　　　　　　：　スポット受信スケジュール管理      *
*    モジュール名　　　　：　スポット受信スケジュ－ル          *
*                                            マスタリスト      *
*    作成日／更新日　　　：　99/09/10                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH0017L.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/09/10.
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
*スポット受信スケジュールマスタ
     SELECT     JHMSJSF    ASSIGN    TO        JHMSJSL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       SJS-F02
                                               SJS-F03
                           FILE      STATUS    SJS-ST.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*プリント定義ファイル
     SELECT     PRTFILE    ASSIGN    TO        LP-04-PRTF
                           FILE      STATUS    PRT-ST.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
****************************************************************
* FILE=スポット受信スケジュールマスタ                          *
****************************************************************
 FD  JHMSJSF
     LABEL       RECORD    IS        STANDARD.
     COPY        JHMSJSF   OF        XFDLIB
     JOINING     SJS       AS        PREFIX.
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
     03  SJS-ST            PIC X(02).
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
     03  SYS-DATE          PIC 9(06) VALUE     ZERO.
     03  RD-SW             PIC 9(01) VALUE     ZERO.
*
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
 01  FILE-ERR.
     03  SJS-ERR           PIC N(20) VALUE
                    NC"スポット受信スケジュールマスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンターエラー".
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1             PIC  9(02).
     03  WK-DATE8-YY2             PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY                  PIC  9(04).
     03  WK-MM                    PIC  9(02).
     03  WK-DD                    PIC  9(02).
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
     03  FILLER                  PIC  X(28)  VALUE  SPACE.
     03  FILLER                  PIC  N(24)  VALUE
         NC"※※　スポット受信スケジュールマスタリスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2                         CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(19)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"日付".
     03  FILLER                  PIC  X(13)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"時間".
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"取引先".
     03  FILLER                  PIC  X(37)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"作成区分".
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE
                                 NC"登録日".
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
*01  SEN1.
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(50)  VALUE
*        "--------------------------------------------------".
*    03  FILLER                  PIC  X(36)  VALUE
*        "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(17)  VALUE  SPACE.
     03  DT1-01                  PIC  9999.
     03  FILLER                  PIC  X(01)  VALUE  "/".
     03  DT1-02                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  "/".
     03  DT1-03                  PIC  99.
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  DT1-04                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-05                  PIC  99.
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  DT1-06                  PIC  ZZZZZZZ9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-07                  PIC  N(15).
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-08                  PIC  X.
     03  FILLER                  PIC  X(01)  VALUE  ":".
     03  DT1-09                  PIC  N(03).
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  DT1-10                  PIC  9999.
     03  FILLER                  PIC  X(01)  VALUE  "/".
     03  DT1-11                  PIC  99.
     03  FILLER                  PIC  X(01)  VALUE  "/".
     03  DT1-12                  PIC  99.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN                 PIC X(01).
 01  LINK-IN-YMD6                PIC 9(06).
 01  LINK-IN-YMD8                PIC 9(08).
 01  LINK-OUT-RET                PIC X(01).
 01  LINK-OUT-YMD                PIC 9(08).
****************************************************************
*             PROCEDURE           DIVISION                     *
****************************************************************
 PROCEDURE                           DIVISION.
 DECLARATIVES.
 SJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMSJSF.
     DISPLAY     SJS-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SJS-ST    UPON      CONS.
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
*ファイルＯＰＥＮ
     OPEN        INPUT     JHMSJSF
                           HTOKMS
                 OUTPUT    PRTFILE.
*初期値セット
     MOVE        ZERO      TO        P-CNT.
     MOVE        SPACE     TO        END-FLG.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC     SECTION.
     MOVE     "MAIN-SEC"          TO          S-NAME.
*改頁チェック
     IF   L-CNT  >= 60
*         改頁
          MOVE          SPACE     TO        PRT-REC
          WRITE   PRT-REC  AFTER  PAGE
          MOVE    ZERO     TO     L-CNT
*
          PERFORM  HEAD-WT-SEC
     END-IF.
*
     PERFORM     FL-READ-SEC.
     IF   END-FLG  NOT = "END"
          PERFORM     BODY-WT-SEC
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
     READ        JHMSJSF   AT        END
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
*             帳票出力処理                            2.2      *
****************************************************************
 BODY-WT-SEC                  SECTION.
     MOVE        "BODY-WT-SEC"    TO   S-NAME.
*    MOVE        SPACE            TO   DT1.
*一件目のみ取引先マスタより名称を取得
     IF       WK-TORICD  =   SJS-F04
              GO             TO    DT-010
     ELSE
          MOVE   SJS-F04     TO    DT1-06  WK-TORICD
     END-IF.
***  取引先ＲＥＡＤ
     MOVE        SJS-F04     TO    TOK-F01.
     READ     HTOKMS
     INVALID
          MOVE   SPACE       TO    DT1-07
     NOT INVALID
          MOVE   TOK-F03     TO    DT1-07
     END-READ.
*
 DT-010.
***  日付
     MOVE     SJS-F02(1:4)   TO        DT1-01.
     MOVE     SJS-F02(5:2)   TO        DT1-02.
     MOVE     SJS-F02(7:2)   TO        DT1-03.
***  時間
     MOVE     SJS-F03(1:2)   TO        DT1-04.
     MOVE     SJS-F03(3:2)   TO        DT1-05.
***  作成区分
     MOVE     SJS-F01        TO        DT1-08.
     IF       SJS-F01   =    1
              MOVE      NC"未作成"     TO   DT1-09
     ELSE
              MOVE      NC"作成済"     TO   DT1-09
     END-IF.
***  登録日
     MOVE     SJS-F05(1:4)   TO        DT1-10.
     MOVE     SJS-F05(5:2)   TO        DT1-11.
     MOVE     SJS-F05(7:2)   TO        DT1-12.
*
*明細部出力
     WRITE    PRT-REC   FROM      DT1       AFTER  1.
*
     ADD      1         TO        L-CNT.
 BODY-WT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                        2.3      *
****************************************************************
 HEAD-WT-SEC                  SECTION.
     MOVE     "HEAD-WT-SEC"       TO   S-NAME.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
***  プログラムＩＤ
     MOVE     "SJH0017L"          TO        HD1-01.
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
     ADD      5            TO        L-CNT.
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                                3.0      *
****************************************************************
 END-SEC                   SECTION.
     MOVE        "END-SEC"           TO          S-NAME.
     CLOSE       PRTFILE    JHMSJSF    HTOKMS.
 END-EXIT.
     EXIT.

```
