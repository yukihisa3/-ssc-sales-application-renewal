# SKE0070I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0070I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　（株）サカタのタネ　　　　　　　　　　*
*    業務名　　　　　：　マスタメンテ                          *
*    モジュール名　　：　検品グループマスタ保守                *
*    作成日／更新日　：　2000/10/11                            *
*    作成者／更新者　：　NAV･ASSIST                            *
*    更新日／更新者　：                                        *
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SKE0070I.
 AUTHOR.                   NAV.
 DATE-WRITTEN.             00/10/11.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          GP6000.
 OBJECT-COMPUTER.          GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*検品グループマスタ
     SELECT      SOKKPGF   ASSIGN    TO        DA-01-VI-SOKKPGL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       KPG-F01
                                               KPG-F02
                           FILE      STATUS    KPG-ST.
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
 FD  SOKKPGF
     LABEL       RECORD    IS        STANDARD.
     COPY        SOKKPGF   OF        XFDLIB
     JOINING     KPG       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*画面ファイル
 FD  DSPFILE.
     COPY        FKE00701    OF      XMDLIB.
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
                           NC"処理区分が違います".
     03  MSG04             PIC N(20) VALUE
                           NC"Ｙで入力して下さい".
     03  MSG05             PIC N(20) VALUE
                           NC"倉庫ＣＤを入力して下さい".
     03  MSG06             PIC N(20) VALUE
                           NC"倉庫Ｍ未登録です".
     03  MSG07             PIC N(20) VALUE
                           NC"検品Ｇコードを入力して下さい".
     03  MSG08             PIC N(20) VALUE
                           NC"検品Ｇ名称を入力して下さい".
     03  MSG09             PIC N(20) VALUE
                           NC"ピッキング種別が違います".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
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
     03  WK-KENPIN         PIC 9(05) VALUE     ZERO.
     03  CHK-FLG           PIC X(03) VALUE     SPACE.
     03  WK-SYUKKA         PIC X(02) VALUE     ZERO.
     03  ERR-FLG           PIC 9(01) VALUE     ZERO.
     03  WK-HENKAN-TI      PIC 9(04) VALUE     ZERO.
     03  WK-SYORI          PIC 9(01) VALUE     ZERO.
     03  ZSOKMS-INV-FLG    PIC X(03) VALUE     SPACE.
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  KPG-ST            PIC X(02).
     03  SOK-ST            PIC X(02).
*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"検品Ｇマスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
 01  WK-SYS-DATE             PIC  9(08).
 01  FILLER                  REDEFINES   WK-SYS-DATE.
     03  WK-SYS-YY           PIC  9(04).
     03  WK-SYS-MM           PIC  9(02).
     03  WK-SYS-DD           PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
*
 01  LINK-AREA2.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE                   SECTION.
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-DSOKCD            PIC   X(02).
******************************************************************
 PROCEDURE                 DIVISION USING PARA-SOKCD PARA-DSOKCD.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 KPG-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           SOKKPGF.
     DISPLAY     KPG-ST    UPON      CONS.
     DISPLAY     FILE-ERR1 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS.
     DISPLAY     SOK-ST    UPON      CONS.
     DISPLAY     FILE-ERR2 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        4000      TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      CONS.
     DISPLAY     FILE-ERR3 UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
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
     OPEN     INPUT     ZSOKMS
              I-O       SOKKPGF
                        DSPFILE.
     MOVE     ZERO      TO        END-FLG.
*
     ACCEPT   SYS-DATE    FROM  DATE.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   WK-SYS-DATE
     ELSE
         MOVE ZERO           TO   WK-SYS-DATE
     END-IF.
     ACCEPT      SYS-TIME2   FROM  TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-INIT-SEC.
*継続処理
     IF   CHK-FLG = "CHK"
          MOVE   WK-SYORI  TO        SYORI
          MOVE  "SYORI"    TO        DSP-GRP
          PERFORM     DSP-WR-SEC
          GO               TO        MAIN-020
     END-IF.
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
         MOVE    MSG03     TO        MSG1
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
*    倉庫ＣＤ、検品Ｇ入力
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-RD-SEC.
     EVALUATE    DSP-FNC
       WHEN
        "F004"
           MOVE    SPACE   TO        CHK-FLG
           GO              TO        MAIN-SEC
       WHEN
        "F010"
           IF  (SYORI  =  2)  AND  (ZI-FLG = ZERO)
               IF  (INV-SW = 1)
                   MOVE      SYUKKA    TO        KPG-F01
                   MOVE      KENPIN    TO        KPG-F02
                   START     SOKKPGF  KEY IS  >  KPG-F01  KPG-F02
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  SOKKPGF    NEXT
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
*    出荷場所 CHK
     IF  (SYUKKA  =  SPACE)
         MOVE    MSG05     TO        MSG1
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
         MOVE    MSG06     TO        MSG1
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
*検品ＧコードCHK
     IF ( KENPIN  NOT  NUMERIC )
     OR ( KENPIN  =    ZERO    )
         MOVE    MSG07     TO        MSG1
         MOVE   "MSG1"     TO        DSP-GRP
         PERFORM DSP-WR-SEC
         MOVE   "R"        TO        EDIT-OPTION  OF  KENPIN
         MOVE   "C"        TO        EDIT-CURSOR  OF  KENPIN
         MOVE   "GRPKEY"   TO        DSP-GRP
         PERFORM DSP-WR-SEC
         GO                TO        MAIN-020
     ELSE
         MOVE   "M"        TO        EDIT-OPTION  OF  KENPIN
         MOVE    SPACE     TO        EDIT-CURSOR  OF  KENPIN
     END-IF.
*    検品グループマスタチェック
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        SYUKKA    TO        KPG-F01.
     MOVE        KENPIN    TO        KPG-F02.
     READ  SOKKPGF
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "M"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE    SPACE     TO        EDIT-CURSOR  OF  SYUKKA
             MOVE   "M"        TO        EDIT-OPTION  OF  KENPIN
             MOVE    SPACE     TO        EDIT-CURSOR  OF  KENPIN
             GO                TO        MAIN-030
         ELSE
             MOVE    MSG01     TO        MSG1
             MOVE    1         TO        INV-SW
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "C"        TO        EDIT-CURSOR  OF  SYUKKA
             MOVE   "R"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE   "R"        TO        EDIT-OPTION  OF  KENPIN
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
         END-IF
       NOT INVALID
         IF  (SYORI  =  1)
             MOVE    MSG02     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "C"        TO        EDIT-CURSOR  OF  SYUKKA
             MOVE   "R"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE   "R"        TO        EDIT-OPTION  OF  KENPIN
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  SYUKKA
             MOVE    SPACE     TO        EDIT-CURSOR  OF  SYUKKA
             MOVE   "M"        TO        EDIT-OPTION  OF  KENPIN
             MOVE    SPACE     TO        EDIT-CURSOR  OF  KENPIN
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYUKKA.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYUKKA.
     MOVE       "M"        TO        EDIT-OPTION  OF  KENPIN.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  KENPIN.
     MOVE        SPACE     TO        MSG1.
*検品グループマスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-KPG1-SEC
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
                    MOVE  SPACE    TO  CHK-FLG
                    GO             TO  MAIN-SEC
         WHEN      "E000"
                             CONTINUE
         WHEN      OTHER
                             MOVE    MSG10   TO   MSG1
                             MOVE   "MSG1"   TO   DSP-GRP
                             PERFORM DSP-WR-SEC
                             GO        TO   MAIN-030
     END-EVALUATE.
     MOVE     SPACE          TO   MSG1.
*    検品グループ名称
     IF   KENMEI   =     SPACE
          MOVE     "R"        TO   EDIT-OPTION  OF  KENMEI
          MOVE     "C"        TO   EDIT-CURSOR  OF  KENMEI
          MOVE      MSG08     TO   MSG1
          MOVE     "MSG1"     TO   DSP-GRP
          PERFORM   DSP-WR-SEC
          MOVE     "GRP001"   TO   DSP-GRP
          PERFORM   DSP-WR-SEC
          GO   TO   MAIN-030
        ELSE
          MOVE     "M"        TO   EDIT-OPTION  OF  KENMEI
          MOVE      SPACE     TO   EDIT-CURSOR  OF  KENMEI
     END-IF.
*    ピッキング種別
     IF   PIKING   =     SPACE
          MOVE     "R"        TO   EDIT-OPTION  OF  PIKING
          MOVE     "C"        TO   EDIT-CURSOR  OF  PIKING
          MOVE      MSG09     TO   MSG1
          MOVE     "MSG1"     TO   DSP-GRP
          PERFORM   DSP-WR-SEC
          MOVE     "GRP001"   TO   DSP-GRP
          PERFORM   DSP-WR-SEC
          GO   TO   MAIN-030
        ELSE
          IF   PIKING  =  "1"  OR  "2"
               MOVE     "M"        TO   EDIT-OPTION  OF  PIKING
               MOVE      SPACE     TO   EDIT-CURSOR  OF  PIKING
          ELSE
               MOVE     "R"        TO   EDIT-OPTION  OF  PIKING
               MOVE     "C"        TO   EDIT-CURSOR  OF  PIKING
               MOVE      MSG09     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
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
                             MOVE  SPACE   TO   CHK-FLG
                             GO             TO   MAIN-SEC
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
           MOVE    MSG04     TO   MSG1
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
     PERFORM  DSP-KPG2-SEC.
 MAIN-080.
     EVALUATE   SYORI
         WHEN     1
              MOVE     WK-SYS-DATE  TO  KPG-F05
              MOVE     SYS-TIMEW    TO  KPG-F06
              WRITE    KPG-REC
              END-WRITE
         WHEN     2
              REWRITE   KPG-REC
              END-REWRITE
         WHEN     3
              DELETE    SOKKPGF
              END-DELETE
     END-EVALUATE.
     MOVE   "SCREEN"        TO      DSP-GRP.
     PERFORM DSP-WR-SEC.
     MOVE    SYORI          TO      WK-SYORI.
     MOVE    "CHK"          TO      CHK-FLG.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             ZSOKMS  SOKKPGF  DSPFILE.
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
*    倉庫ＣＤチェック
     IF       PARA-DSOKCD  = "01"  OR  "88"
              MOVE    " "      TO   EDIT-STATUS OF SYUKKA
     ELSE
              MOVE PARA-SOKCD  TO   SOK-F01 SYUKKA
              READ ZSOKMS
                   INVALID     MOVE "INV" TO ZSOKMS-INV-FLG
                   NOT INVALID MOVE SPACE TO ZSOKMS-INV-FLG
              END-READ
              IF  ZSOKMS-INV-FLG = SPACE
                  MOVE SOK-F02    TO SYUMEI
              ELSE
                  MOVE ALL NC"＊" TO SYUMEI
              END-IF
              MOVE "X"     TO   EDIT-STATUS OF SYUKKA
     END-IF.
     WRITE      FKE00701.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FKE00701.
*    倉庫ＣＤチェック
     IF       PARA-DSOKCD  = "01"  OR  "88"
              MOVE    " "      TO   EDIT-STATUS OF SYUKKA
     ELSE
              MOVE PARA-SOKCD  TO   SOK-F01 SYUKKA
              READ ZSOKMS
                   INVALID     MOVE "INV" TO ZSOKMS-INV-FLG
                   NOT INVALID MOVE SPACE TO ZSOKMS-INV-FLG
              END-READ
              IF  ZSOKMS-INV-FLG = SPACE
                  MOVE SOK-F02    TO SYUMEI
              ELSE
                  MOVE ALL NC"＊" TO SYUMEI
              END-IF
              MOVE "X"     TO   EDIT-STATUS OF SYUKKA
     END-IF.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FKE00701"       TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     WRITE   FKE00701.
     INITIALIZE              FKE00701.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*    検品グループマスタのレコードを画面に転送                  *
****************************************************************
 DSP-KPG1-SEC              SECTION.
*    倉庫ＣＤ
     MOVE     KPG-F01        TO        SYUKKA   WK-SYUKKA.
     PERFORM  SOK-RD-SEC.
*    検品グループ
     MOVE     KPG-F02        TO        KENPIN.
*    検品グループ名称／ピッキング種別
     MOVE     KPG-F03        TO        KENMEI.
     MOVE     KPG-F04        TO        PIKING.
*
 DSP-KPG1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-KPG2-SEC           SECTION.
     IF       SYORI     =    1
              MOVE      SPACE     TO   KPG-REC
              INITIALIZE               KPG-REC
     END-IF.
     MOVE     SYUKKA         TO        KPG-F01.
     MOVE     KENPIN         TO        KPG-F02.
     MOVE     KENMEI         TO        KPG-F03.
     MOVE     PIKING         TO        KPG-F04.
*
     MOVE     WK-SYS-DATE    TO        KPG-F07.
     MOVE     SYS-TIMEW      TO        KPG-F08.
 DSP-KPG2-EXIT.
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

```
