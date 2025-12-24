# SIT0070L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SIT0070L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　                        *
*    業務名　　　　　　　：　店舗マスタ　　　　　　　　        *
*    モジュール名　　　　：　店舗マスタ保守　　                *
*    作成日／作成者　　　：　92/11/10  /Y.Y                    *
*    更新日／更新者　　　：　95/04/14  /NAV･ASSIST             *
*            項目追加（出荷場所，ルート，ルート順番）          *
*    更新日／更新者　　　：　99/09/28  /HAGIWARA               *
*            新郵便番号追加                                    *
*            日付８桁変換ＰＧ（ＳＫＹＤＴＣＫＢ）使用          *
*    更新日／更新者　　　：　03/04/25  /NAV･ASSIST             *
*            項目追加（支店営業所区分）                        *
*    更新日／更新者　　　：　03/07/24  /NAV･ASSIST             *
*            項目追加（対象外区分）                            *
*    更新日／更新者　　　：　07/02/05  /NAV･ASSIST             *
*            項目追加（振分倉庫ＣＤ）                          *
*    更新日／更新者　　　：　09/03/17  /NAV･ASSIST             *
*             ＩＴ統制対応                                     *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SIT0070I.
 AUTHOR.                   S.I.
 DATE-WRITTEN.             09/03/17.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-150SI.
 OBJECT-COMPUTER.          K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*店舗マスタ
     SELECT      HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TEN-F52
                                               TEN-F011
                           FILE      STATUS    TEN-ST.
*取引先マスタ
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST.
*マスタ更新履歴ファイル
     SELECT     MSTLOGF    ASSIGN    TO        DA-01-VI-MSTLOGL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       MSL-F01
                                               MSL-F02
                                               MSL-F03
                                               MSL-F04
                                               MSL-F05
                                               MSL-F06
                                               MSL-F07
                           FILE      STATUS    MSL-ST.
*画面ファイル*
     SELECT      DSPFILE   ASSIGN    TO        GS-DSPF
                           SYMBOLIC  DESTINATION        "DSP"
                           DESTINATION-1       DSP-WS
                           FORMAT              DSP-FMT
                           GROUP               DSP-GRP
                           PROCESSING  MODE    DSP-PRO
                           UNIT      CONTROL   DSP-UNIT
                           SELECTED  FUNCTION  DSP-FNC
                           FILE      STATUS    DSP-ST.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*店舗マスタ
 FD  HTENMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     BLOCK       CONTAINS   6        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*マスタ更新履歴ファイル
 FD  MSTLOGF
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        MSTLOGF   OF        XFDLIB
     JOINING     MSL       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FIT00701  OF        XMDLIB.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
 01  DSP-AREA.
     03  DSP-FMT           PIC X(08) VALUE     SPACE.
     03  DSP-GRP           PIC X(08) VALUE     SPACE.
     03  DSP-WS            PIC X(08) VALUE     SPACE.
     03  DSP-WSR           REDEFINES DSP-WS.
         05  DSP-WS1       PIC X(02).
         05  DSP-WS2       PIC 9(03).
         05  DSP-WS3       PIC X(01).
     03  DSP-PRO           PIC X(02) VALUE     SPACE.
     03  DSP-UNIT          PIC X(06) VALUE     SPACE.
     03  DSP-FNC           PIC X(04) VALUE     SPACE.
     03  DSP-ST            PIC X(02) VALUE     SPACE.
     03  DSP-ST1           PIC X(04) VALUE     SPACE.
 01  MSG-AREA.
     03  MSG01             PIC N(20) VALUE
                           NC"登録されていません".
     03  MSG02             PIC N(20) VALUE
                           NC"すでに登録されています".
     03  MSG03             PIC N(20) VALUE
                           NC"正式名を入力してください".
     03  MSG05             PIC N(20) VALUE
                           NC"処理区分が違います".
     03  MSG06             PIC N(20) VALUE
                           NC"Ｙで入力してください".
     03  MSG07             PIC N(20) VALUE
                           NC"取引先ＣＤを入力してください".
     03  MSG08             PIC N(20) VALUE
                           NC"店舗ＣＤを入力してください".
     03  MSG09             PIC N(20) VALUE
                           NC"取引先Ｍが登録されていません".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
     03  MSG11             PIC N(20) VALUE
                           NC"種別が未入力です".
     03  MSG12             PIC N(20) VALUE
                           NC"出荷場所が未登録です".
     03  MSG13             PIC N(20) VALUE
                           NC"ルートが未入力です".
     03  MSG14             PIC N(20) VALUE
                           NC"ルート順番が未入力です".
     03  MSG15             PIC N(20) VALUE
                           NC"支店／営業所区分が未入力です".
     03  MSG16             PIC N(20) VALUE
                           NC"支店／営業所区分を入力して下さい".
     03  MSG17             PIC N(20) VALUE
                   NC"対象外区分は、空白又は１で入力して下さい".
     03  MSG18             PIC N(20) VALUE
                   NC"ケーヨー→１～２　カインズ→１～６".
     03  MSG19             PIC N(20) VALUE
                   NC"ダイキＨＣ　店＝空白　センター＝１".
     03  MSG20             PIC N(20) VALUE
                   NC"振分倉庫ＣＤが条件Ｆ未登録です。".
*
     03  PMSG01            PIC N(20) VALUE
                           NC"_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_次検索".
     03  PMSG03            PIC N(20) VALUE
                           NC"_取消".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_再入力".
*01  SYS-DATE              PIC 9(08).
 01  WSYS-DATE.
     03  WSYS-Y1           PIC 9(02).
     03  WSYS-YMD.
         05  WSYS-YY       PIC 9(02).
         05  WSYS-MM       PIC 9(02).
         05  WSYS-DD       PIC 9(02).
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
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  WK-SYUKKA         PIC X(08) VALUE     SPACE.
     03  ERR-FLG           PIC 9(01) VALUE     ZERO.
     03  WK-SYORI          PIC 9(01) VALUE     ZERO.
     03  HJYOKEN-INV-FLG   PIC X(03) VALUE     SPACE.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  TEN-ST            PIC X(02).
     03  TEN-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  JYO-ST1           PIC X(04).
     03  MSL-ST            PIC X(02).
     03  MSL-ST1           PIC X(04).
*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"店舗マスタ異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"取引先マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR4         PIC N(10) VALUE
                                  NC"条件ファイル異常！".
     03  FILE-ERR5         PIC N(12) VALUE
                                  NC"マスタ更新履歴Ｆ異常！".
*---------------- 99/09/28追加 --------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(510).
*
 01  SEQ                   PIC  9(02).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
 01  PARA-UPDTDATE         PIC 9(08).
 01  PARA-UPDTIME          PIC 9(06).
*--------------------------------------------------------*
******************************************************************
 PROCEDURE                 DIVISION  USING PARA-BUMONCD
                                           PARA-TANCD
                                           PARA-UPDTDATE
                                           PARA-UPDTIME.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTENMS.
     DISPLAY     TEN-ST    UPON      CONS.
**** DISPLAY     TEN-ST1   UPON      CONS.
     DISPLAY     FILE-ERR1 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTOKMS.
     DISPLAY     TOK-ST    UPON      CONS.
**** DISPLAY     TOK-ST1   UPON      CONS.
     DISPLAY     FILE-ERR3 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
*    追加95/04/14
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HJYOKEN.
     DISPLAY     JYO-ST    UPON      CONS.
     DISPLAY     FILE-ERR4 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 MSL-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE MSTLOGF.
     DISPLAY     MSL-ST    UPON      CONS.
     DISPLAY     FILE-ERR5 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      CONS.
**** DISPLAY     DSP-ST1   UPON      CONS.
     DISPLAY     FILE-ERR2 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 SHORI-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC  UNTIL     END-FLG   =   9.
     PERFORM     END-SEC.
     STOP        RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*                      初期処理
******************************************************************
 INIT-SEC                  SECTION.
     OPEN        INPUT     HTOKMS  HJYOKEN
                 I-O       HTENMS  MSTLOGF
                           DSPFILE.
     MOVE        ZERO      TO        END-FLG.
     MOVE        SPACE     TO        TEN-REC.
     INITIALIZE  TEN-REC.
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
*----------------------------------------------------*
*受渡しパラメタセット
     MOVE     DATE-AREA    TO        PARA-UPDTDATE.
     MOVE     WK-TIME(1:6) TO        PARA-UPDTIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-INIT-SEC.
*処理区分入力
 MAIN-010.
     MOVE        PMSG01    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE       "SYORI"    TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F005"
           MOVE     9      TO        END-FLG
           GO              TO        MAIN-EXIT
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*処理区分 CHK
     IF  (SYORI  =  1 OR 2 OR 3)
         MOVE    SPACE     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"        TO        EDIT-OPTION  OF  SYORI
         MOVE    SPACE     TO        EDIT-CURSOR  OF  SYORI
         MOVE   "SYORI"    TO        DSP-GRP
         PERFORM DSP-WR-SEC
       ELSE
         MOVE    MSG05     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  SYORI
         MOVE   "C"        TO        EDIT-CURSOR  OF  SYORI
         MOVE   "SYORI"    TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-010.
 MAIN-020.
*--< ＰＦキー選択 >--*
     IF  (SYORI  =  2)
          MOVE   PMSG02    TO        MSG2
     ELSE
          MOVE   PMSG03    TO        MSG2
     END-IF.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*    取引先，店舗キー入力
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           GO              TO        MAIN-SEC
       WHEN
        "F010"
           IF  (SYORI  =  2)  AND  (ZI-FLG = ZERO)
               IF  (INV-SW = 1)
                   MOVE      TORICD    TO        TEN-F52
                   MOVE      TENPCD    TO        TEN-F011
                   START     HTENMS  KEY IS  >  TEN-F52  TEN-F011
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  HTENMS    NEXT
                 AT  END
                   MOVE NC"次レコード無し"     TO        MSG1
                   MOVE   "MSG1"     TO        DSP-GRP
                   PERFORM DSP-WR-SEC
                   MOVE    1         TO        ZI-FLG
                   GO                TO        MAIN-020
                 NOT AT END
                   MOVE    ZERO      TO        INV-SW
                   GO      TO        MAIN-000
               END-READ
           END-IF
           IF  (SYORI  =  2)
                MOVE NC"次レコード無し" TO     MSG1
           ELSE
                MOVE    MSG10        TO        MSG1
           END-IF
           MOVE   "MSG1"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO              TO        MAIN-020
       WHEN
        "E000"
           CONTINUE
       WHEN
         OTHER
           MOVE    MSG10   TO        MSG1
           MOVE   "MSG1"   TO        DSP-GRP
           PERFORM DSP-WR-SEC
           GO              TO        MAIN-020
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*取引先コード CHK
     IF  (TORICD  =  ZERO)
         MOVE    MSG07     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC.
*
     MOVE        TORICD    TO        TOK-F01.
     READ  HTOKMS
       INVALID
         MOVE    MSG09     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
       NOT INVALID
         MOVE    TOK-F03   TO        TORINM
         MOVE    SPACE     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
     END-READ.
*店舗コード CHK
     IF  (TENPCD  =  ZERO)
         MOVE    MSG08     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TENPCD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC.
*
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        TORICD    TO        TEN-F52.
     MOVE        TENPCD    TO        TEN-F011.
     READ  HTENMS
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-030
         ELSE
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TENPCD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         IF  (SYORI  =  1)
             MOVE    MSG02     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
             MOVE   "C"        TO        EDIT-CURSOR  OF  TENPCD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  TORICD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TORICD.
     MOVE       "M"        TO        EDIT-OPTION  OF  TENPCD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TENPCD.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE        SPACE     TO        MSG1.
     MOVE       "MSG1"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*店舗マスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-TEN1-SEC
         MOVE       "GRPKEY"   TO        DSP-GRP
         PERFORM     DSP-WR-SEC
         MOVE       "GRP001"   TO        DSP-GRP
         PERFORM     DSP-WR-SEC.
     IF  (SYORI  =  3)
         GO                TO        MAIN-040.
 MAIN-030.
     MOVE        PMSG03    TO        MSG2.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*項目 入力
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                    MOVE   SYORI   TO  WK-SYORI
                    MOVE   SPACE   TO  FIT00701
                    MOVE   WK-SYORI TO  SYORI
                    MOVE  "SCREEN" TO  DSP-GRP
                    PERFORM DSP-WR-SEC
                    INITIALIZE     TORICD  TORINM  TENPCD
                                   TNAME   TKANA   TRYAKU
                                   TKANAR  YUBIN   JYU1
                                   JYU2    TEL     FAX
                                   R002    SYUBT   SYUKKA
                                   RUTO    RUTONO  EIGYO
                                   NYBN1   NYBN2   TAISYO
                    GO             TO  MAIN-020
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-030
     END-EVALUATE.
     MOVE     SPACE          TO   MSG1.
     MOVE     "MSG1"         TO   DSP-GRP.
     PERFORM  DSP-WR-SEC.
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM  DSP-WR-SEC.
*正式名
     IF  (TNAME  =  SPACE)
         MOVE     "R"        TO   EDIT-OPTION  OF  TNAME
         MOVE     "C"        TO   EDIT-CURSOR  OF  TNAME
         MOVE      MSG03     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  TNAME
         MOVE      SPACE     TO   EDIT-CURSOR  OF  TNAME
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*
     IF  (TORICD   =         173)     AND
         (SYUBT    =         SPACE)
         MOVE     "R"        TO   EDIT-OPTION  OF  SYUBT
         MOVE     "C"        TO   EDIT-CURSOR  OF  SYUBT
         MOVE      MSG11     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  SYUBT
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUBT
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*%%95/04/14%%%%%%%%%%%%%%%%%%%追加%%%%%
*項目追加のため（出荷場所，ルート，ルート順番）
*    出荷場所入力チェック
     IF  (SYUKKA   =   SPACE)
**       MOVE     "R"        TO   EDIT-OPTION  OF  SYUKKA
**       MOVE     "C"        TO   EDIT-CURSOR  OF  SYUKKA
**       MOVE      MSG11     TO   MSG1
**       MOVE     "MSG1"     TO   DSP-GRP
**       PERFORM   DSP-WR-SEC
**       MOVE     "GRP001"   TO   DSP-GRP
**       PERFORM   DSP-WR-SEC
**       GO   TO   MAIN-030
         MOVE      SPACE     TO   SYUMEI
       ELSE
         MOVE      ZERO      TO   ERR-FLG
         MOVE      SYUKKA    TO   WK-SYUKKA
         PERFORM   JYO-RD-SEC
         IF        ERR-FLG   =  1
                   MOVE  "R"     TO   EDIT-OPTION  OF  SYUKKA
                   MOVE  "C"     TO   EDIT-CURSOR  OF  SYUKKA
                   MOVE  MSG12   TO   MSG1
                   MOVE  "MSG1"  TO   DSP-GRP
                   PERFORM   DSP-WR-SEC
                   MOVE     "GRP001"   TO   DSP-GRP
                   PERFORM   DSP-WR-SEC
                   GO   TO   MAIN-030
         END-IF
         MOVE      "M"       TO   EDIT-OPTION  OF  SYUKKA
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SYUKKA
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*    ルート
     IF  (RUTO     NOT   NUMERIC)
     OR  (RUTO     =     ZERO   )
          IF SYUKKA NOT = SPACE
             MOVE     "R"        TO   EDIT-OPTION  OF  RUTO
             MOVE     "C"        TO   EDIT-CURSOR  OF  RUTO
             MOVE      MSG13     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
          END-IF
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  RUTO
         MOVE      SPACE     TO   EDIT-CURSOR  OF  RUTO
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*    ルート順番
     IF  (RUTONO   NOT   NUMERIC)
     OR  (RUTO     =     ZERO   )
          IF SYUKKA NOT = SPACE
             MOVE     "R"        TO   EDIT-OPTION  OF  RUTONO
             MOVE     "C"        TO   EDIT-CURSOR  OF  RUTONO
             MOVE      MSG14     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
          END-IF
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  RUTONO
         MOVE      SPACE     TO   EDIT-CURSOR  OF  RUTONO
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*    売場コード
     IF  (RUTONO   NOT   NUMERIC)
         MOVE     ZERO       TO   URIBA
     END-IF.
*****支店/営業所区分
     IF    (TORICD   =     8737 OR 87371)
         IF  (EIGYO    NOT   NUMERIC)  OR  (EIGYO    =   ZERO)
              MOVE     "R"        TO   EDIT-OPTION  OF  EIGYO
              MOVE     "C"        TO   EDIT-CURSOR  OF  EIGYO
              MOVE      MSG15     TO   MSG1
              MOVE     "MSG1"     TO   DSP-GRP
              PERFORM   DSP-WR-SEC
              GO   TO   MAIN-030
         ELSE
              IF (EIGYO = 1 OR 2 OR 3 OR 4 OR 5 OR 6)
                  MOVE     "M"        TO   EDIT-OPTION  OF  EIGYO
                  MOVE      SPACE     TO   EDIT-CURSOR  OF  EIGYO
                  MOVE     "MSG1"     TO   DSP-GRP
                  PERFORM   DSP-WR-SEC
              ELSE
                  MOVE     "R"        TO   EDIT-OPTION  OF  EIGYO
                  MOVE     "C"        TO   EDIT-CURSOR  OF  EIGYO
                  MOVE      MSG16     TO   MSG1
                  MOVE     "MSG1"     TO   DSP-GRP
                  PERFORM   DSP-WR-SEC
                  GO   TO   MAIN-030
              END-IF
         END-IF
     ELSE
              MOVE     ZERO       TO   EIGYO
              MOVE     "M"        TO   EDIT-OPTION  OF  EIGYO
              MOVE      SPACE     TO   EDIT-CURSOR  OF  EIGYO
              MOVE     "GRP001"   TO   DSP-GRP
              PERFORM   DSP-WR-SEC
     END-IF.
*    対象外区分
     IF   TAISYO   NOT =     SPACE
          IF TAISYO  NOT =  "1"
             MOVE     "R"        TO   EDIT-OPTION  OF  TAISYO
             MOVE     "C"        TO   EDIT-CURSOR  OF  TAISYO
             MOVE      MSG17     TO   MSG1
             MOVE     "MSG1"     TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             MOVE     "GRP001"   TO   DSP-GRP
             PERFORM   DSP-WR-SEC
             GO   TO   MAIN-030
          END-IF
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  TAISYO
         MOVE      SPACE     TO   EDIT-CURSOR  OF  TAISYO
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*    センター区分（ケーヨーの場合）
     IF   TORICD  =  173
       IF   CENTER   NOT =     SPACE
            IF CENTER  NOT =  "1" AND "2"
               MOVE     "R"        TO   EDIT-OPTION  OF  CENTER
               MOVE     "C"        TO   EDIT-CURSOR  OF  CENTER
               MOVE      MSG18     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
            ELSE
               MOVE     "M"        TO   EDIT-OPTION  OF  CENTER
               MOVE      SPACE     TO   EDIT-CURSOR  OF  CENTER
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
            END-IF
         ELSE
               MOVE     "R"        TO   EDIT-OPTION  OF  CENTER
               MOVE     "C"        TO   EDIT-CURSOR  OF  CENTER
               MOVE      MSG18     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
       END-IF
     END-IF.
*    センター区分（カインズの場合）
     IF   TORICD  =  921084
       IF   CENTER   NOT =     SPACE
            MOVE     "23"      TO     JYO-F01
            MOVE      CENTER   TO     JYO-F02
            PERFORM   HJYOKEN-READ-SEC
            IF HJYOKEN-INV-FLG = "INV"
               MOVE     "R"        TO   EDIT-OPTION  OF  CENTER
               MOVE     "C"        TO   EDIT-CURSOR  OF  CENTER
               MOVE      MSG18     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
            ELSE
               MOVE     "M"        TO   EDIT-OPTION  OF  CENTER
               MOVE      SPACE     TO   EDIT-CURSOR  OF  CENTER
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
            END-IF
         ELSE
               MOVE     "R"        TO   EDIT-OPTION  OF  CENTER
               MOVE     "C"        TO   EDIT-CURSOR  OF  CENTER
               MOVE      MSG18     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
       END-IF
     END-IF.
*    センター区分（ダイキの場合）
     IF   TORICD  =  100403  OR  100441  OR  100427
                  OR 100404  OR  100442  OR  100428
            IF CENTER  NOT =  " " AND "1"
               MOVE     "R"        TO   EDIT-OPTION  OF  CENTER
               MOVE     "C"        TO   EDIT-CURSOR  OF  CENTER
               MOVE      MSG19     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
            ELSE
               MOVE     "M"        TO   EDIT-OPTION  OF  CENTER
               MOVE      SPACE     TO   EDIT-CURSOR  OF  CENTER
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
            END-IF
     END-IF.
*    振分倉庫CD
     IF  FURSOK   =   SPACE
         MOVE      SPACE     TO   FURMEI
         MOVE     "M"        TO   EDIT-OPTION  OF  FURSOK
         MOVE      SPACE     TO   EDIT-CURSOR  OF  FURSOK
         MOVE     "GRP001"   TO   DSP-GRP
     ELSE
         MOVE      ZERO      TO      ERR-FLG
         MOVE     "20"       TO      JYO-F01
         MOVE      FURSOK    TO      JYO-F02
         PERFORM   HJYOKEN-READ-SEC
         IF HJYOKEN-INV-FLG = "INV"
            MOVE     "R"        TO   EDIT-OPTION  OF  FURSOK
            MOVE     "C"        TO   EDIT-CURSOR  OF  FURSOK
            MOVE      MSG20     TO   MSG1
            MOVE     "MSG1"     TO   DSP-GRP
            MOVE ALL NC"＊"     TO   FURMEI
            PERFORM   DSP-WR-SEC
            MOVE     "GRP001"   TO   DSP-GRP
            PERFORM   DSP-WR-SEC
            GO   TO   MAIN-030
         ELSE
            MOVE     "M"        TO   EDIT-OPTION  OF  FURSOK
            MOVE      SPACE     TO   EDIT-CURSOR  OF  FURSOK
            MOVE     "GRP001"   TO   DSP-GRP
            MOVE      JYO-F03   TO   FURMEI
            PERFORM   DSP-WR-SEC
         END-IF
     END-IF.
*
     MOVE     "Y"            TO   R002.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
 MAIN-040.
*確認入力
     MOVE      PMSG04        TO   MSG2.
     MOVE     "MSG2"         TO   DSP-GRP.
     PERFORM   DSP-WR-SEC.
     MOVE     "R002"         TO   DSP-GRP.
     PERFORM   DSP-RD-SEC.
     EVALUATE    DSP-FNC
         WHEN      "F004"
                             MOVE   SYORI   TO   WK-SYORI
                             MOVE   SPACE   TO   FIT00701
                             MOVE   WK-SYORI TO   SYORI
                             MOVE  "SCREEN" TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE     TORICD  TORINM  TENPCD
                                            TNAME   TKANA   TRYAKU
                                            TKANAR  YUBIN   JYU1
                                            JYU2    TEL     FAX
                                            R002    SYUBT   SYUKKA
                                            RUTO    RUTONO  URIBA
                                            NYBN1   NYBN2   EIGYO
                                            TAISYO  FURSOK
                             GO             TO   MAIN-020
         WHEN      "F009"
           MOVE     SPACE    TO        R002
           MOVE    "R002"    TO        DSP-GRP
           PERFORM  DSP-WR-SEC
           IF  (SYORI  = 1 OR 2)
                             GO        TO   MAIN-030
           ELSE
                             GO        TO   MAIN-040
           END-IF
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO   TO   MAIN-040
     END-EVALUATE.
     PERFORM     DSP-WR-SEC.
*
     IF  (R002  NOT =  "Y")
           MOVE    MSG06     TO   MSG1
           MOVE   "MSG1"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO          TO   MAIN-040
     ELSE
           MOVE    SPACE     TO   MSG1
           MOVE   "MSG1"     TO   DSP-GRP
           PERFORM DSP-WR-SEC
     END-IF.
*
     IF  (SYORI  =  3)
           GO          TO   MAIN-080.
*画面から転送
     PERFORM  DSP-TEN2-SEC.
 MAIN-080.
     EVALUATE   SYORI
         WHEN     1
              MOVE     SYS-DATE   TO  TEN-F98
              MOVE     TEN-REC    TO  DATA-TAIHI
              WRITE    TEN-REC
              END-WRITE
         WHEN     2
              MOVE     TEN-REC    TO  DATA-TAIHI
              REWRITE  TEN-REC
              END-REWRITE
         WHEN     3
              MOVE     TEN-REC    TO  DATA-TAIHI
              DELETE   HTENMS
              END-DELETE
     END-EVALUATE.
*
     PERFORM  MSTLOGF-WRITE-SEC.
*
     MOVE    SYORI          TO      WK-SYORI.
     MOVE    SPACE          TO      FIT00701.
     MOVE    WK-SYORI       TO      SYORI.
     MOVE   "SCREEN"        TO      DSP-GRP.
     PERFORM DSP-WR-SEC.
     INITIALIZE     TORICD  TORINM  TENPCD  TNAME   TKANA   TRYAKU
                    TKANAR  YUBIN   JYU1    JYU2    TEL     FAX
                    R002    SYUBT   SYUKKA  RUTO    RUTONO
                    NYBN1   NYBN2   FURSOK.
     GO             TO      MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             HTENMS  HJYOKEN
                       DSPFILE.
 END-EXIT.
     EXIT.
****************************************************************
*    画面　　ＲＥＡＤ　　                                      *
****************************************************************
 DSP-RD-SEC             SECTION.
     MOVE  "NE"               TO  DSP-PRO.
     READ                         DSPFILE.
 DSP-RD-EXIT.
     EXIT.
****************************************************************
*    画面    ＷＲＩＴＥ                                      *
****************************************************************
 DSP-WR-SEC             SECTION.
     MOVE       SPACE        TO  DSP-PRO.
     WRITE      FIT00701.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FIT00701.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FIT00701"          TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FIT00701.
     INITIALIZE              FIT00701.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*            商品Ｍのレコードを画面に転送            *
****************************************************************
 DSP-TEN1-SEC              SECTION.
     MOVE     TEN-F52        TO        TORICD.
     MOVE     TEN-F011       TO        TENPCD.
     MOVE     TEN-F02        TO        TNAME.
     MOVE     TEN-F04        TO        TKANA.
     MOVE     TEN-F03        TO        TRYAKU.
     MOVE     TEN-F55        TO        TKANAR.
     MOVE     TEN-F781       TO        NYBN1.
     MOVE     TEN-F782       TO        NYBN2.
     MOVE     TEN-F05        TO        YUBIN.
     MOVE     TEN-F06        TO        JYU1.
     MOVE     TEN-F07        TO        JYU2.
     MOVE     TEN-F08        TO        TEL.
     MOVE     TEN-F09        TO        FAX.
     MOVE     TEN-F97        TO        SYUBT.
     MOVE     TEN-F79        TO        URIBA.
     MOVE     TEN-F80        TO        SYUKKA   WK-SYUKKA.
     PERFORM  JYO-RD-SEC.
     MOVE     TEN-F81        TO        RUTO.
     MOVE     TEN-F82        TO        RUTONO.
     MOVE     TEN-F77        TO        EIGYO.
     MOVE     TEN-F76        TO        TAISYO.
     MOVE     TEN-F75        TO        CENTER.
     MOVE     TEN-F73        TO        FURSOK.
*
 DSP-TEN1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-TEN2-SEC           SECTION.
     MOVE     TORICD         TO        TEN-F52.
     MOVE     TENPCD         TO        TEN-F011.
     MOVE     TNAME          TO        TEN-F02.
     MOVE     TKANA          TO        TEN-F04.
     MOVE     TRYAKU         TO        TEN-F03.
     MOVE     TKANAR         TO        TEN-F55.
     MOVE     NYBN1          TO        TEN-F781.
     MOVE     NYBN2          TO        TEN-F782.
     MOVE     YUBIN          TO        TEN-F05.
     MOVE     JYU1           TO        TEN-F06.
     MOVE     JYU2           TO        TEN-F07.
     MOVE     TEL            TO        TEN-F08.
     MOVE     FAX            TO        TEN-F09.
     MOVE     URIBA          TO        TEN-F79.
     MOVE     SYUKKA         TO        TEN-F80.
     MOVE     RUTO           TO        TEN-F81.
     MOVE     RUTONO         TO        TEN-F82.
     MOVE     SYUBT          TO        TEN-F97.
     MOVE     EIGYO          TO        TEN-F77.
     MOVE     TAISYO         TO        TEN-F76.
     MOVE     CENTER         TO        TEN-F75.
     MOVE     FURSOK         TO        TEN-F73.
*
     MOVE     SYS-DATE       TO        TEN-F99.
 DSP-TEN2-EXIT.
     EXIT.
****************************************************************
*  条件ファイルより出荷場所取得     95/04/14 追加            *
****************************************************************
 JYO-RD-SEC             SECTION.
*
     MOVE     20             TO        JYO-F01.
     MOVE     WK-SYUKKA      TO        JYO-F02.
     READ     HJYOKEN        INVALID
              MOVE ALL NC"　" TO       SYUMEI
              MOVE 1         TO        ERR-FLG
     NOT  INVALID
              MOVE JYO-F03   TO        SYUMEI
              MOVE ZERO      TO        ERR-FLG
     END-READ.
*    出荷場所名表示
     MOVE      "SCREEN"      TO        DSP-GRP.
     PERFORM   DSP-WR-SEC.
*
 JYO-RD-EXIT.
     EXIT.
****************************************************************
*  条件ファイルルート参照
****************************************************************
 HJYOKEN-READ-SEC       SECTION.
*
     READ     HJYOKEN        INVALID
              MOVE "INV"     TO        HJYOKEN-INV-FLG
     NOT  INVALID
              MOVE SPACE     TO        HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*            マスタ更新履歴ファイル出力            *
****************************************************************
 MSTLOGF-WRITE-SEC           SECTION.
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
*
     MOVE     "03"                TO   MSL-F01.
     MOVE     PARA-BUMONCD        TO   MSL-F02.
     MOVE     PARA-TANCD          TO   MSL-F03.
     MOVE     SYORI               TO   MSL-F04.
     MOVE     DATE-AREA           TO   MSL-F05.
     MOVE     WK-TIME(1:6)        TO   MSL-F06.
     START    MSTLOGF            KEY IS >= MSL-F01
                                           MSL-F02
                                           MSL-F03
                                           MSL-F04
                                           MSL-F05
                                           MSL-F06
              INVALID
              MOVE     "03"          TO    MSL-F01
              MOVE     PARA-BUMONCD  TO    MSL-F02
              MOVE     PARA-TANCD    TO    MSL-F03
              MOVE     SYORI         TO    MSL-F04
              MOVE     DATE-AREA     TO    MSL-F05
              MOVE     WK-TIME(1:6)  TO    MSL-F06
              MOVE     0             TO  MSL-F07
              MOVE     DATA-TAIHI    TO  MSL-F08
              WRITE    MSL-REC
              GO TO    MSTLOGF-WRITE-EXIT
     END-START.
     MOVE     0                   TO   SEQ.
*
 MSTLOG-WRITE-010.
     READ   MSTLOGF  NEXT  AT        END
            GO             TO        MSTLOGF-WRITE-EXIT
     END-READ.
*
     IF  (MSL-F01 > "03")       OR (MSL-F02 > PARA-BUMONCD) OR
         (MSL-F03 > PARA-TANCD) OR (MSL-F04 > SYORI)      OR
         (MSL-F05 > DATE-AREA)  OR (MSL-F06 > WK-TIME(1:6))
         MOVE     "03"             TO  MSL-F01
         MOVE     PARA-BUMONCD     TO  MSL-F02
         MOVE     PARA-TANCD       TO  MSL-F03
         MOVE     SYORI            TO  MSL-F04
         MOVE     DATE-AREA        TO  MSL-F05
         MOVE     WK-TIME(1:6)     TO  MSL-F06
         MOVE     SEQ              TO  MSL-F07
         MOVE     DATA-TAIHI       TO  MSL-F08
         WRITE    MSL-REC
         GO       TO               MSTLOGF-WRITE-EXIT
     ELSE
         COMPUTE  SEQ =  MSL-F07  +  1
         GO           TO             MSTLOG-WRITE-010
     END-IF.
 MSTLOGF-WRITE-EXIT.
     EXIT.

```
