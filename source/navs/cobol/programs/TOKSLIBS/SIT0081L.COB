****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　                        *
*    業務名　　　　　　　：　店舗マスタ　　　　　　　　        *
*    モジュール名　　　　：　店舗マスタリスト　                *
*    作成日／作成者　　　：　92/11/10  /Y.Y                    *
*    更新日／更新者　　　：　99/09/28  /HAGIWARA               *
*            新郵便番号追加                                    *
*            日付８桁変換ＰＧ（ＳＫＹＤＴＣＫＢ）使用          *
*    更新日／更新者　　　：　09/03/17  /IMAI                   *
*            ＩＴ統制対応                                      *
*    処理概要　　　　　　：　FORM未使用
*    更新日／更新者　　　：　11/03/25  /TAKAHASHI              *
*    処理概要　　　　　　：　改頁行数を変更　６０＝＞５５
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0081L.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/04/27.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     STATION     IS        STA
         YA        IS   NIHONGO
         YB        IS   YB
         YB-21     IS   YB-21.
***********************************************************
*             INPUT-OUTPUT                                *
***********************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取引先マスタ
     SELECT     HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        MSTLOGL3
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      SEQUENTIAL
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                     FILE      STATUS    MSL-ST.
*担当者マスタ
     SELECT     HTANMS     ASSIGN    TO        TANMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TAN-F01
                                               TAN-F02
                           FILE      STATUS    TAN-ST.
*%* プリンター *%*
     SELECT     PRINTF     ASSIGN    TO        LP-04.
*
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*担当者マスタ
 FD  HTANMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTANMS    OF        XFDLIB
     JOINING     TAN       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*プリンター
 FD  PRINTF      LINAGE    IS        66.
 01  P-REC                 PIC  X(200).
******************************************************************
 WORKING-STORAGE           SECTION.
******************************************************************
*%*表示パラメータ*%*
 01  FORM-PARA.
     03  DSP-FORM          PIC X(08).
     03  DSP-PROC          PIC X(02).
     03  DSP-GRP           PIC X(08).
     03  PF-KEY            PIC X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL     PIC X(04).
         05  DSP-STR-PG    PIC X(02).
     03  PRT-FORM          PIC X(08).
     03  PRT-PROC          PIC X(02).
     03  PRT-GRP           PIC X(08).
     03  PRT-CTL.
         05  PRT-CNTRL     PIC X(04).
         05  PRT-STR-PG    PIC X(02).
*
 01  FILE-STATUS.
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  PRT-ST            PIC X(02).
     03  PRT-ST1           PIC X(04).
     03  TAN-ST            PIC X(02).
     03  TAN-ST1           PIC X(04).
*システム日付
*01  SYS-DATE              PIC 9(06).
*01  SYS-DATE8.
*    03  WSYS-YY           PIC 9(04).
*    03  WSYS-MM           PIC 9(02).
*    03  WSYS-DD           PIC 9(02).
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
****  見出し行１             ****
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SIT0081L".
     02  FILLER              PIC  X(29)  VALUE  SPACE.
     02  FILLER              PIC  N(15)
         CHARACTER  TYPE  IS  YB-21      VALUE
         NC"＊＊　店舗マスタ　リスト　＊＊".
     02  FILLER              PIC  X(20)  VALUE  SPACE.
     02  FILLER              PIC  X(05)  VALUE  "DATE:".
     02  ODATE               PIC  X(10).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  FILLER              PIC  X(05)  VALUE  "PAGE".
     02  PAGE-SUU            PIC  ZZ9.
****  見出し行１２　１４　１９***
 01  MIDASI-12.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(22)  VALUE
         "+------+------+------+".
****  見出し行１３           ****
 01  MIDASI-13.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"責任者".
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  N(03)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"担当者".
     02  FILLER              PIC  X(01)  VALUE  "!".
***  見出し行１５ １６ １７ １８
 01  MIDASI-15.
     02  FILLER              PIC  X(112) VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
     02  FILLER              PIC  X(06)  VALUE  SPACE.
     02  FILLER              PIC  X(01)  VALUE  "!".
*
****  見出し行２             ****
 01  MIDASI-2       CHARACTER     TYPE   IS   NIHONGO.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"取引先".
     02  FILLER              PIC  X(02)  VALUE  "CD".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"取引先名".
     02  FILLER              PIC  X(10)  VALUE  SPACE.
     02  FILLER              PIC  N(02)  VALUE
         NC"店舗".
     02  FILLER              PIC  X(02)  VALUE  "CD".
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"店舗名".
     02  FILLER              PIC  X(37)  VALUE  SPACE.
     02  FILLER              PIC  N(01)  VALUE
         NC"〒".
     02  FILLER              PIC  X(07)  VALUE  SPACE.
     02  FILLER              PIC  N(03)  VALUE
         NC"住　所".
     02  FILLER              PIC  X(18)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"ＴＥＬ／ＦＡＸ".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"店舗区分".
     02  FILLER              PIC  X(09)  VALUE  SPACE.
****  見出し行２１           ****
 01  MIDASI-21      CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  N(07)  VALUE
         NC"［処理：担当者".
     02  FILLER              PIC  X(14)  VALUE  SPACE.
     02  FILLER              PIC  N(04)  VALUE
         NC"：更新日".
     02  FILLER              PIC  X(08)  VALUE  SPACE.
     02  FILLER              PIC  N(05)  VALUE
         NC"更新時刻］".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
****  明細行１               ****
 01  MEISAI-1       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TORICD              PIC  X(08).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TORINM              PIC  N(10).
     02  FILLER              PIC  X(03)  VALUE  SPACE.
     02  TENPCD              PIC  9(05).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  TNAME               PIC  N(16).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  TKANA               PIC  X(15).
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  NYBN1               PIC  X(03).
     02  FILLER              PIC  X(01)  VALUE  "-".
     02  NYBN2               PIC  X(04).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  JYU1                PIC  N(16).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TEL                 PIC  X(12).
     02  FILLER              PIC  X(05)  VALUE  SPACE.
     02  KBN                 PIC  X(01).
****  明細行２               ****
 01  MEISAI-2       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(87)  VALUE  SPACE.
     02  JYU2                PIC  N(16).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FAX                 PIC  X(12).
****  明細行３               ****
 01  MEISAI-3       CHARACTER     TYPE   IS   YB.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  UDKBN               PIC  N(04).
     02  TANCD               PIC  X(02).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  TANNM               PIC  N(11).
     02  UPDT                PIC  X(10).
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  UPTM                PIC  X(08).
     02  FILLER              PIC  N(01)  VALUE  NC"］".
*
 01  WK-AREA.
     03  END-SW            PIC 9(01) VALUE     ZERO.
     03  ERR-SW            PIC 9(01) VALUE     ZERO.
     03  PAGE-CNT          PIC 9(05) VALUE     ZERO.
     03  LINE-CNT          PIC 9(05) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
*
 01  READ-CNT              PIC  9(07) VALUE 0.
 01  IN-CNT                PIC  9(07) VALUE 0.
 01  IN-DATA               PIC X(01).
 01  FILE-ERR.
     03  MSL-ERR           PIC  N(11) VALUE
                        NC"マスタ更新履歴Ｆエラー".
     03  TAN-ERR           PIC N(10) VALUE
                        NC"担当者マスタエラー".
     03  TOK-ERR           PIC N(10) VALUE
                        NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(10) VALUE
                        NC"プリンターエラー".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　終了が開始より小さい".
     03  WK-ERR2                  PIC  N(12) VALUE
              NC"　　『店舗マスタ作成中』".
     03  WK-ERR4                  PIC  N(12) VALUE
              NC"　対象データがありません".
*---------------- 99/09/28追加 --------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
*帳票　更新履歴編集
 01  HEN-KUBUN.
     03  FILLER                   PIC  N(01)  VALUE NC"［".
     03  HEN-KUBUNNM              PIC  N(02).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
 01  HEN-TAN.
     03  HEN-TANNM                PIC  N(10).
     03  FILLER                   PIC  N(01)  VALUE NC"：".
*担当者コード
 01  WK-TANCD.
     03  WK-TANCD1         PIC  X(02).
     03  WK-FILLER         PIC  X(06).
*マスタレコードエリア（店舗マスタ）
     COPY    HTENMS        OF   XFDLIB
     JOINING TEN           AS   PREFIX.
*更新範囲
 01  TRND-DT.
     03  TRND-DATE         PIC  9(08).
     03  TRND-TIME         PIC  9(06).
 01  TO-DT.
     03  TO-DATE           PIC  9(08).
     03  TO-TIME           PIC  9(06).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
 01  PARA-UPDTDATE-E       PIC 9(08).
 01  PARA-UPDTIME-E        PIC 9(06).
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE                 DIVISION USING PARA-BUMONCD
                                          PARA-TANCD
                                          PARA-UPDTDATE
                                          PARA-UPDTIME
                                          PARA-UPDTDATE-E
                                          PARA-UPDTIME-E.
 DECLARATIVES.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ERR   UPON      STA.
     DISPLAY     MSL-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      STA.
     DISPLAY     TAN-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      STA.
     DISPLAY     TOK-ST    UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
*****MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC                  SECTION.
*
 PROC-010.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-SW = 9  OR 1
     PERFORM     END-SEC.
     IF  (END-SW = 1)
         GO               TO         PROC-010.
*
     STOP        RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC                  SECTION.
     MOVE        ZERO      TO        PAGE-CNT.
     MOVE        61        TO        LINE-CNT.
*----------------- 99/09/28追加 ---------------------*
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*パラメタ：担当者コード取得
     MOVE     PARA-TANCD          TO   WK-TANCD.
*----------------------------------------------------*
     OPEN        INPUT     MSTLOGF  HTANMS
                           HTOKMS
                 OUTPUT    PRINTF.
*マスタ更新履歴ファイル初期ＲＥＡＤ
     MOVE     "03"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02
     MOVE     WK-TANCD            TO   MSL-F03.
     MOVE     PARA-UPDTDATE       TO   MSL-F05.
     MOVE     PARA-UPDTIME        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F05
                                           MSL-F06
              INVALID  MOVE  9    TO   END-SW
                       GO TO      INIT-EXIT.
     PERFORM  READMSL-SEC.
*
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC                  SECTION.
*2011/03/25 行数変更 60=>55 ST
*****IF  (LINE-CNT  >  60)
     IF  (LINE-CNT  >  55)
*2011/03/25 行数変更 60=>55 ED
          PERFORM MIDA-SEC
     END-IF.
     PERFORM     MEIEDT-SEC.
     PERFORM     MEIWRT-SEC.
     PERFORM     READMSL-SEC.
*
 MAIN-EXIT.
     EXIT.
**********************
*マスタ更新履歴ファイルＲＥＡＤ*
**********************
 READMSL-SEC               SECTION.
     READ   MSTLOGF  NEXT  AT        END
         MOVE    9         TO        END-SW
         GO                TO        READMSL-EXIT.
*
     MOVE   MSL-F05        TO        TRND-DATE.
     MOVE   MSL-F06        TO        TRND-TIME.
     MOVE   PARA-UPDTDATE-E TO       TO-DATE.
     MOVE   PARA-UPDTIME-E  TO       TO-TIME.
*
     IF  (MSL-F01  >  "03") OR (MSL-F02  >  PARA-BUMONCD) OR
         (MSL-F03  >  PARA-TANCD) OR (TRND-DT > TO-DT)
         MOVE    9         TO        END-SW
         GO      TO        READMSL-EXIT.
     ADD         1         TO        READ-CNT.
*
 READMSL-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                   SECTION.
     CLOSE       PRINTF      MSTLOGF  HTANMS  HTOKMS.
     DISPLAY  "SIT0081L READ =" READ-CNT UPON STA.
     DISPLAY  "SIT0081L IN   =" IN-CNT   UPON STA.
 END-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集書き出し               *
**********************************************************
 MIDA-SEC                  SECTION.
     MOVE        HEN-DATE  TO        ODATE.
     MOVE        ZERO      TO        WK-TORICD.
     MOVE        ZERO      TO        LINE-CNT.
     ADD         1         TO        PAGE-CNT.
     MOVE        PAGE-CNT  TO        PAGE-SUU.
**************
*帳票書き出し*
**************
     IF       PAGE-CNT  = 1
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-13  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-21  AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
        ELSE
              MOVE       SPACE   TO      P-REC
              WRITE      P-REC   AFTER   PAGE
              WRITE      P-REC   FROM    MIDASI-1   AFTER  2
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-13  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-15  AFTER  1
              WRITE      P-REC   FROM    MIDASI-12  AFTER  1
              WRITE      P-REC   FROM    MIDASI-2   AFTER  1
              WRITE      P-REC   FROM    MIDASI-21  AFTER  1
              WRITE      P-REC   FROM    SEN1       AFTER  1
     END-IF.
     MOVE       13         TO        LINE-CNT.
 MIDA-EXIT.
     EXIT.
**********************************************************
*                 明細情報の編集                         *
**********************************************************
 MEIEDT-SEC                  SECTION.
*
     ADD         1         TO        IN-CNT.
     MOVE        MSL-F08   TO        TEN-REC.
     MOVE        TEN-F52   TO        TORICD.
*取引先ＲＥＡＤ
     MOVE        TEN-F52   TO        TOK-F01.
     READ    HTOKMS
     INVALID
          MOVE   SPACE     TO        TORINM
     NOT INVALID
          MOVE   TOK-F03   TO        TORINM
     END-READ.
*
 MEIEDT-010.
     MOVE        TEN-F011  TO        TENPCD.
     MOVE        TEN-F02   TO        TNAME.
     MOVE        TEN-F04   TO        TKANA.
*---------------- 99/09/28追加 -----------------*
     MOVE        TEN-F781  TO        NYBN1.
     MOVE        TEN-F782  TO        NYBN2.
*-----------------------------------------------*
     MOVE     TEN-F781     TO        NYBN1.
     MOVE     TEN-F782     TO        NYBN2.
     MOVE     TEN-F06      TO        JYU1.
     MOVE     TEN-F07      TO        JYU2.
     MOVE     TEN-F08      TO        TEL.
     MOVE     TEN-F09      TO        FAX.
     MOVE     TEN-F97      TO        KBN.
*    MOVE     TEN-F75      TO        CENT.
*更新履歴
     EVALUATE    MSL-F04
         WHEN    "1"
                 MOVE NC"登録"          TO   HEN-KUBUNNM
         WHEN    "2"
                 MOVE NC"修正"          TO   HEN-KUBUNNM
         WHEN    "3"
                 MOVE NC"削除"          TO   HEN-KUBUNNM
     END-EVALUATE.
     MOVE        HEN-KUBUN              TO   UDKBN.
*
     MOVE        MSL-F03                TO   WK-TANCD.
     MOVE        WK-TANCD1              TO   TANCD.
*担当者名取得
     MOVE        MSL-F02                TO   TAN-F01.
     MOVE        WK-TANCD1              TO   TAN-F02.
     READ        HTANMS
       INVALID
                 MOVE   SPACE           TO  HEN-TANNM
       NOT INVALID
                 MOVE   TAN-F03         TO  HEN-TANNM
     END-READ.
*
     MOVE        HEN-TAN                TO  TANNM.
     MOVE        MSL-F05(1:4)           TO  HEN-DATE-YYYY.
     MOVE        MSL-F05(5:2)           TO  HEN-DATE-MM.
     MOVE        MSL-F05(7:2)           TO  HEN-DATE-DD.
     MOVE        HEN-DATE               TO  UPDT.
*
     MOVE        MSL-F06(1:2)           TO  HEN-TIME-HH.
     MOVE        MSL-F06(3:2)           TO  HEN-TIME-MM.
     MOVE        MSL-F06(5:2)           TO  HEN-TIME-SS.
     MOVE        HEN-TIME               TO  UPTM.
*
*    MOVE        NC"］"                 TO  KAKO.
*
 MEIEDT-EXIT.
     EXIT.
**********************************************************
*                    明細データ書き出し                  *
**********************************************************
 MEIWRT-SEC                   SECTION.
**************
*帳票書き出し*
**************
     WRITE    P-REC         FROM    MEISAI-1.
     WRITE    P-REC         FROM    MEISAI-2.
     WRITE    P-REC         FROM    MEISAI-3.
     WRITE    P-REC         FROM    SEN1.
     ADD         4         TO        LINE-CNT.
 MEIWRT-EXIT.
     EXIT.
