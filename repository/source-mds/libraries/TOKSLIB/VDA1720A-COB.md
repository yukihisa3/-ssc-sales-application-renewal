# VDA1720A

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA1720A.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　マスタメンテ                          *
*    モジュール名　　：　店舗別ルートマスタメンテ　　　　　　　*
*    作成日／更新日　：　97/03/14                              *
*    作成者／更新者　：　NAV･ASSIST                            *
*    更新日／更新者　：                                        *
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               VDA1720A.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             97/03/14.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-150SI.
 OBJECT-COMPUTER.          K-150SI.
 SPECIAL-NAMES.
     STATION     IS        STA.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*店舗別ルートマスタ
     SELECT      HTENRMS   ASSIGN    TO        DA-01-VI-TENRMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TER-F01
                                               TER-F02
                                               TER-F03
                           FILE      STATUS    TER-ST.
*店舗マスタ
     SELECT      HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TEN-F52
                                               TEN-F011
                           FILE      STATUS    TEN-ST.
*取引先マスタ
     SELECT      HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*条件ファイル
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01
                                               JYO-F02
                           FILE      STATUS    JYO-ST.
*倉庫マスタ
     SELECT      ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       SOK-F01
                           FILE      STATUS    SOK-ST.
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
*店舗別ルートマスタ
 FD  HTENRMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENRMS   OF        XFDLIB
     JOINING     TER       AS        PREFIX.
*店舗マスタ
 FD  HTENMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTENMS    OF        XFDLIB
     JOINING     TEN       AS        PREFIX.
*取引先マスタ
 FD  HTOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     LABEL       RECORD    IS        STANDARD.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FVDA1720    OF      XMDLIB.
 01  FVDA1720R.
     03  D-SYORI          PIC X(06).
     03  D-KOMOKU         PIC X(350).
     03  D-MSG            PIC X(110).
     03  D-R002           PIC X(06).
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
     03  MSG05             PIC N(20) VALUE
                           NC"処理区分が違います".
     03  MSG06             PIC N(20) VALUE
                           NC"Ｙで入力して下さい".
     03  MSG07             PIC N(20) VALUE
                           NC"取引先ＣＤを入力して下さい".
     03  MSG08             PIC N(20) VALUE
                           NC"店舗ＣＤを入力して下さい".
     03  MSG09             PIC N(20) VALUE
                           NC"取引先Ｍが登録されていません".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
     03  MSG13             PIC N(20) VALUE
                           NC"ルートが未入力です".
     03  MSG14             PIC N(20) VALUE
                           NC"ルート順番が未入力です".
     03  MSG15             PIC N(20) VALUE
                           NC"出荷場所を入力して下さい".
     03  MSG16             PIC N(20) VALUE
                           NC"倉庫Ｍが登録されていません".
     03  MSG17             PIC N(20) VALUE
                           NC"店舗Ｍが登録されていません".
*
     03  PMSG01            PIC N(20) VALUE
                           NC"_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_次検索".
     03  PMSG03            PIC N(20) VALUE
                           NC"_取消".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_再入力".
 01  SYS-DATE              PIC 9(08) VALUE     ZERO.
 01  WSYS-DATE.
     03  WSYS-YMD          PIC 9(06) VALUE     ZERO.
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
     03  WK-TORICD         PIC 9(08) VALUE     ZERO.
     03  WK-TENPCD         PIC 9(05) VALUE     ZERO.
*****03  WK-SYUKKA         PIC 9(02) VALUE     ZERO.
     03  WK-SYUKKA         PIC X(02) VALUE     ZERO.
     03  ERR-FLG           PIC 9(01) VALUE     ZERO.
     03  WK-HENKAN-TI      PIC 9(04) VALUE     ZERO.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  TER-ST            PIC X(02).
     03  TER-ST1           PIC X(04).
     03  TEN-ST            PIC X(02).
     03  TEN-ST1           PIC X(04).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  JYO-ST            PIC X(02).
     03  JYO-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
     03  SOK-ST1           PIC X(04).
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
                                  NC"店舗別ルートマスタ異常！".
     03  FILE-ERR6         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
******************************************************************
 PROCEDURE                 DIVISION.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 TER-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTENRMS.
     DISPLAY     TER-ST    UPON      STA.
     DISPLAY     FILE-ERR5 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTENMS.
     DISPLAY     TEN-ST    UPON      STA.
     DISPLAY     FILE-ERR1 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTOKMS.
     DISPLAY     TOK-ST    UPON      STA.
     DISPLAY     FILE-ERR3 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HJYOKEN.
     DISPLAY     JYO-ST    UPON      STA.
     DISPLAY     FILE-ERR4 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS.
     DISPLAY     SOK-ST    UPON      STA.
     DISPLAY     FILE-ERR6 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 SHORI-SEC                 SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL     END-FLG   =    9.
     PERFORM  END-SEC.
     STOP     RUN.
 SHORI-EXIT.
     EXIT.
******************************************************************
*                      初期処理
******************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     HTOKMS    HJYOKEN   HTENMS    ZSOKMS
              I-O       HTENRMS
                        DSPFILE.
     MOVE     ZERO      TO        END-FLG.
     MOVE     SPACE     TO        TER-REC.
     INITIALIZE                   TER-REC.
*
     PERFORM  JYO-RD-SEC.
     ACCEPT   WSYS-YMD  FROM      DATE.
     COMPUTE  SYS-DATE  =    WK-HENKAN-TI  *  10000  +  WSYS-YMD.
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
         GO                TO        MAIN-010
     END-IF.
 MAIN-020.
*--< ＰＦキー選択 >--*
     IF  (SYORI  =  2)
          MOVE   PMSG02    TO        MSG2
     ELSE
          MOVE   PMSG03    TO        MSG2
     END-IF.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*    取引先，店舗，出荷場所キー入力
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
                   MOVE      TORICD    TO        TER-F01
                   MOVE      SYUKKA    TO        TER-F02
                   MOVE      TENPCD    TO        TER-F03
                   START     HTENRMS  KEY IS  >  TER-F01  TER-F02
                                                 TER-F03
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  HTENRMS    NEXT
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
*取引先コード CHK
     IF  (TORICD  =  ZERO)
         MOVE    MSG07     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        TORINM
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
     END-IF.
*
     MOVE        TORICD    TO        TOK-F01.
     READ  HTOKMS
       INVALID
         MOVE    MSG09     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        TORINM
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
       NOT INVALID
         MOVE    TOK-F02   TO        TORINM
         MOVE    SPACE     TO        MSG1
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
     END-READ.
*    出荷場所 CHK
*****IF  (SYUKKA  =  ZERO)
     IF  (SYUKKA  =  SPACE)
         MOVE    MSG15     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        SYUMEI
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  SYUKKA
         MOVE   "C"        TO        EDIT-CURSOR  OF  SYUKKA
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  SYUKKA
         MOVE    SPACE     TO        EDIT-CURSOR  OF  SYUKKA
     END-IF.
*
     MOVE        SYUKKA    TO        SOK-F01.
     READ  ZSOKMS
       INVALID
         MOVE    MSG16     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        SYUMEI
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  SYUKKA
         MOVE   "C"        TO        EDIT-CURSOR  OF  SYUKKA
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
       NOT INVALID
         MOVE    SOK-F02   TO        SYUMEI
         MOVE    SPACE     TO        MSG1
         MOVE   "M"        TO        EDIT-OPTION  OF  SYUKKA
         MOVE    SPACE     TO        EDIT-CURSOR  OF  SYUKKA
     END-READ.
*店舗コード CHK
     IF  (TENPCD  =  ZERO)
         MOVE    MSG08     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        TNAME
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TENPCD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
     END-IF.
*
     MOVE        TORICD    TO        TEN-F52.
     MOVE        TENPCD    TO        TEN-F011.
     READ  HTENMS
       INVALID
         MOVE    MSG17     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         MOVE    SPACE     TO        TNAME
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
         MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
         MOVE   "C"        TO        EDIT-CURSOR  OF  TENPCD
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
       NOT INVALID
         MOVE    TEN-F02   TO        TNAME
         MOVE    SPACE     TO        MSG1
         MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
         MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
         MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
     END-READ.
*    店舗別ルートマスタチェック
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        TORICD    TO        TER-F01.
     MOVE        SYUKKA    TO        TER-F02.
     MOVE        TENPCD    TO        TER-F03.
     READ  HTENRMS
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "M"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE    SPACE     TO        EDIT-CURSOR  OF  SYUKKA
             MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
             GO                TO        MAIN-030
         ELSE
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         IF  (SYORI  =  1)
             MOVE    MSG02     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "C"        TO        EDIT-CURSOR  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  TORICD
             MOVE   "R"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE   "R"        TO        EDIT-OPTION  OF  TENPCD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "M"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE    SPACE     TO        EDIT-CURSOR  OF  SYUKKA
             MOVE   "M"        TO        EDIT-OPTION  OF  TENPCD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TENPCD
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  TORICD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TORICD.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYUKKA.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYUKKA.
     MOVE       "M"        TO        EDIT-OPTION  OF  TENPCD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TENPCD.
     MOVE        SPACE     TO        MSG1.
*店舗別ルートマスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-TER1-SEC
     END-IF.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     IF  (SYORI  =  3)
         GO                TO        MAIN-040
     END-IF.
 MAIN-030.
     MOVE        PMSG03    TO        MSG2.
     MOVE       "SCREEN"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*項目 入力
     MOVE       "GRP001"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE      DSP-FNC
         WHEN      "F004"
                    MOVE   SPACE   TO  D-KOMOKU  D-R002
                    MOVE  "SCREEN" TO  DSP-GRP
                    PERFORM DSP-WR-SEC
                    INITIALIZE     TORICD  TORINM  TENPCD
                                   TNAME   SYUKKA  SYUMEI
                                   RUTO    RUTONO
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
*    ルート
     IF  (RUTO     NOT   NUMERIC)
     OR  (RUTO     =     ZERO   )
         MOVE     "R"        TO   EDIT-OPTION  OF  RUTO
         MOVE     "C"        TO   EDIT-CURSOR  OF  RUTO
         MOVE      MSG13     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  RUTO
         MOVE      SPACE     TO   EDIT-CURSOR  OF  RUTO
     END-IF.
*    ルート順番
     IF  (RUTONO   NOT   NUMERIC)
     OR  (RUTO     =     ZERO   )
         MOVE     "R"        TO   EDIT-OPTION  OF  RUTONO
         MOVE     "C"        TO   EDIT-CURSOR  OF  RUTONO
         MOVE      MSG14     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  RUTONO
         MOVE      SPACE     TO   EDIT-CURSOR  OF  RUTONO
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
                             MOVE   SPACE   TO   D-KOMOKU  D-R002
                             MOVE  "SCREEN" TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE     TORICD  TORINM  TENPCD
                                            TNAME   SYUKKA  SYUMEI
                                            RUTO    RUTONO
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
     PERFORM  DSP-TER2-SEC.
 MAIN-080.
     EVALUATE   SYORI
         WHEN     1
              MOVE     SYS-DATE   TO  TER-F98
              WRITE    TER-REC
              END-WRITE
         WHEN     2
              REWRITE   TER-REC
              END-REWRITE
         WHEN     3
              DELETE    HTENRMS
              END-DELETE
     END-EVALUATE.
     MOVE    SPACE          TO      D-KOMOKU  D-R002.
     MOVE   "SCREEN"        TO      DSP-GRP.
     PERFORM DSP-WR-SEC.
     INITIALIZE     TORICD  TORINM  TENPCD  TNAME   SYUKKA  SYUMEI
                    RUTO    RUTONO.
     GO             TO      MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             HTENMS     HJYOKEN   ZSOKMS
                       HTENRMS    DSPFILE.
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
     WRITE      FVDA1720.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FVDA1720.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FVDA1720"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     WRITE   FVDA1720.
     INITIALIZE              FVDA1720.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*    店舗別ルートマスタのレコードを画面に転送                  *
****************************************************************
 DSP-TER1-SEC              SECTION.
     MOVE     TER-F01        TO        TORICD  WK-TORICD.
     PERFORM  TOK-RD-SEC.
*
     MOVE     TER-F02        TO        SYUKKA   WK-SYUKKA.
     PERFORM  SOK-RD-SEC.
*
     MOVE     TER-F03        TO        TENPCD  WK-TENPCD.
     PERFORM  TEN-RD-SEC.
*
     MOVE     TER-F04        TO        RUTO.
     MOVE     TER-F05        TO        RUTONO.
*
 DSP-TER1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-TER2-SEC           SECTION.
     MOVE     TORICD         TO        TER-F01.
     MOVE     SYUKKA         TO        TER-F02.
     MOVE     TENPCD         TO        TER-F03.
     MOVE     RUTO           TO        TER-F04.
     MOVE     RUTONO         TO        TER-F05.
*
     MOVE     SYS-DATE       TO        TER-F99.
 DSP-TER2-EXIT.
     EXIT.
****************************************************************
*  取引先マスタより取引先名取得                                *
****************************************************************
 TOK-RD-SEC             SECTION.
*
     MOVE     WK-TORICD      TO        TOK-F01.
     READ     HTOKMS         INVALID
              MOVE ALL NC"＊" TO       TORINM
              MOVE 1         TO        ERR-FLG
     NOT  INVALID
              MOVE TOK-F02   TO        TORINM
              MOVE ZERO      TO        ERR-FLG
     END-READ.
*
 TOK-RD-EXIT.
     EXIT.
****************************************************************
*  倉庫マスタより倉庫名取得                                    *
****************************************************************
 SOK-RD-SEC             SECTION.
     MOVE     WK-SYUKKA      TO        SOK-F01.
     READ     ZSOKMS         INVALID
              MOVE ALL NC"＊" TO       SYUMEI
              MOVE 1         TO        ERR-FLG
     NOT  INVALID
              MOVE SOK-F02   TO        SYUMEI
              MOVE ZERO      TO        ERR-FLG
     END-READ.
 SOK-RD-EXIT.
     EXIT.
****************************************************************
*  店舗マスタより店舗名取得                                    *
****************************************************************
 TEN-RD-SEC             SECTION.
     MOVE     WK-TORICD      TO        TEN-F52.
     MOVE     WK-TENPCD      TO        TEN-F011.
     READ     HTENMS         INVALID
              MOVE ALL NC"＊" TO       TNAME
              MOVE 1         TO        ERR-FLG
     NOT  INVALID
              MOVE TEN-F02   TO        TNAME
              MOVE ZERO      TO        ERR-FLG
     END-READ.
 TEN-RD-EXIT.
     EXIT.
****************************************************************
*  条件ファイルより西暦変換取得                                *
****************************************************************
 JYO-RD-SEC             SECTION.
*条件Ｆ（西暦変換）
     MOVE     "57"           TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     READ     HJYOKEN        INVALID
              DISPLAY "JYOKEN READ ERR KEY=57 "  UPON STA
     NOT  INVALID
              MOVE JYO-F04   TO        WK-HENKAN-TI
     END-READ.
*
 JYO-RD-EXIT.
     EXIT.

```
