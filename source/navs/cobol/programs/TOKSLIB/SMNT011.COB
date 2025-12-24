****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　　　　　　                        *
*    業務名　　　　　　　：　取引先マスタ　　　　　　　        *
*    モジュール名　　　　：　取引先マスタ保守　                *
*    作成日／作成者　　　：　92/11/10  /Y.Y                    *
*    更新日／更新者　　　：　99/09/27  /HAGIWARA               *
*    更新内容　　　　　　：　新郵便番号追加                    *
*    　　　　　　　　　　　　代表倉庫（倉庫マスタＲＥＡＤ）追加*
*    　　　　　　　　　　　　自社指定伝票タイプ                *
*    　　　　　　　　　　　　税計算区分　　　　　　　　　　　　*
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION            DIVISION.
 PROGRAM-ID.               SMNT011.
 AUTHOR.                   Y.Y.
 DATE-WRITTEN.             92/11/10.
 ENVIRONMENT               DIVISION.
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.          K-150SI.
 OBJECT-COMPUTER.          K-150SI.
 SPECIAL-NAMES.
     STATION     IS        STA
     CONSOLE     IS        CONS.
***************************************************************
 INPUT-OUTPUT              SECTION.
***************************************************************
 FILE-CONTROL.
*取引先マスタ
     SELECT      HTOKMS    ASSIGN    TO        TOKMS2
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      DYNAMIC
                           RECORD    KEY       TOK-F01
                           FILE      STATUS    TOK-ST.
*倉庫マスタ
     SELECT     ZSOKMS     ASSIGN    TO        ZSOKMS1
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
*取引先マスタ
 FD  HTOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        HTOKMS    OF        XFDLIB
     JOINING     TOK       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS
     BLOCK       CONTAINS   8        RECORDS
     LABEL       RECORD    IS        STANDARD.
     COPY        ZSOKMS    OF        XFDLIB
     JOINING     SOK       AS        PREFIX.
*ＤＩＳＰＬＡＹ
 FD  DSPFILE.
     COPY        FM011    OF        XMDLIB.
*
 01  FM011R.
*****03  D-SYORI           PIC X(06).
     03  D-SYORI           PIC X(34).
     03  D-KOMOKU          PIC X(474).
     03  D-MSG             PIC X(110).
     03  D-R002            PIC X(06).
**************************************458*************************
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
     03  MSG04             PIC N(20) VALUE
                           NC"伝発区分エラー".
     03  MSG05             PIC N(20) VALUE
                           NC"処理区分が違います".
     03  MSG06             PIC N(20) VALUE
                           NC"税端数区分エラー".
     03  MSG07             PIC N(20) VALUE
                           NC"ＣＨＫＤ区分エラー".
     03  MSG08             PIC N(20) VALUE
                           NC"締日を入力してください".
     03  MSG09             PIC N(20) VALUE
                           NC"取引先ＣＤを入力してください".
     03  MSG10             PIC N(20) VALUE
                           NC"ＰＦキーが違います".
     03  MSG11             PIC N(20) VALUE
                           NC"Ｙで入力してください".
     03  MSG12             PIC N(20) VALUE
                           NC"ＣＨＫＤ２区分エラー".
     03  MSG13             PIC N(20) VALUE
                           NC"付番区分エラー".
     03  MSG14             PIC N(20) VALUE
                           NC"倉庫マスタに存在しません".
     03  MSG15             PIC N(20) VALUE
                           NC"自社指定伝票タイプエラー".
     03  MSG16             PIC N(20) VALUE
                           NC"税計算区分エラー".
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
 01  WK-AREA.
     03  END-FLG           PIC 9(01).
     03  INV-SW            PIC 9(01) VALUE     ZERO.
     03  ZI-FLG            PIC 9(01) VALUE     ZERO.
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
 01  ST-AREA.
     03  IN-DATA           PIC X(01).
     03  TOK-ST            PIC X(02).
     03  TOK-ST1           PIC X(04).
     03  SOK-ST            PIC X(02).
*
     03  FILE-ERR1         PIC N(10) VALUE
                                  NC"得意先マスタ異常！".
     03  FILE-ERR2         PIC N(10) VALUE
                                  NC"画面ファイル異常！".
     03  FILE-ERR3         PIC N(10) VALUE
                                  NC"倉庫マスタ異常！".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
******************************************************************
 PROCEDURE                 DIVISION.
******************************************************************
*                     ファイルエラー処理
******************************************************************
 DECLARATIVES.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           HTOKMS.
     DISPLAY     TOK-ST    UPON      STA.
**** DISPLAY     TOK-ST1   UPON      STA.
     DISPLAY     FILE-ERR1 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           ZSOKMS.
     DISPLAY     SOK-ST    UPON      STA.
**** DISPLAY     TOK-ST1   UPON      STA.
     DISPLAY     FILE-ERR3 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
**** MOVE        255       TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE           DSPFILE.
     DISPLAY     DSP-ST    UPON      STA.
**** DISPLAY     DSP-ST1   UPON      STA.
     DISPLAY     FILE-ERR2 UPON      STA.
     ACCEPT      IN-DATA   FROM      STA.
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
     OPEN        I-O       HTOKMS
                           DSPFILE.
     OPEN        INPUT     ZSOKMS.
*
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
*
     MOVE        ZERO      TO        END-FLG.
     MOVE        SPACE     TO        TOK-REC.
     INITIALIZE  TOK-REC.
*
*    ACCEPT     WSYS-YMD   FROM      DATE.
*    IF   (WSYS-YY  >  92)
*          MOVE     20     TO        WSYS-Y1
*      ELSE
*          MOVE     19     TO        WSYS-Y1
*    END-IF.
*    MOVE        WSYS-DATE TO        SYS-DATE.
 INIT-EXIT.
     EXIT.
****************************************************************
*                      メイン処理                              *
****************************************************************
 MAIN-SEC                  SECTION.
     PERFORM     DSP-INIT-SEC.
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
           MOVE  PMSG02    TO        MSG2
     ELSE
           MOVE  PMSG03    TO        MSG2
     END-IF.
     MOVE       "MSG2"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
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
                   MOVE        TORICD    TO        TOK-F01
                   START     HTOKMS  KEY IS  >   TOK-F01
                   INVALID  KEY
                        MOVE      NC"次レコード無し"  TO MSG1
                        MOVE        "MSG1"     TO        DSP-GRP
                        PERFORM      DSP-WR-SEC
                        MOVE         1         TO        ZI-FLG
                        MOVE         1         TO        INV-SW
                        GO                     TO        MAIN-020
                   END-START
               END-IF
               READ  HTOKMS    NEXT
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
                MOVE NC"次レコード無し"     TO        MSG1
           ELSE
                MOVE   MSG10          TO        MSG1
           END-IF
           MOVE       "MSG1"          TO        DSP-GRP
           PERFORM     DSP-WR-SEC
           GO                         TO        MAIN-020
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
 MAIN-001.
     IF  (TORICD  =  ZERO)
         MOVE    MSG09     TO        MSG1
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
     MOVE        ZERO      TO        ZI-FLG.
     MOVE        TORICD    TO        TOK-F01.
     READ  HTOKMS
       INVALID
         IF  (SYORI  =  1)
             MOVE    1         TO        INV-SW
             MOVE    SPACE     TO        MSG1
             MOVE   "MSG1"     TO        DSP-GRP
             PERFORM DSP-WR-SEC
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
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
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
             GO                TO        MAIN-020
          ELSE
             MOVE   "M"        TO        EDIT-OPTION  OF  TORICD
             MOVE    SPACE     TO        EDIT-CURSOR  OF  TORICD
             MOVE   "GRPKEY"   TO        DSP-GRP
             PERFORM DSP-WR-SEC
         END-IF
     END-READ.
 MAIN-000.
     MOVE       "M"        TO        EDIT-OPTION  OF  SYORI.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  SYORI.
     MOVE       "M"        TO        EDIT-OPTION  OF  TORICD.
     MOVE        SPACE     TO        EDIT-CURSOR  OF  TORICD.
     MOVE       "GRPKEY"   TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
     MOVE        SPACE     TO        MSG1.
     MOVE       "MSG1"     TO        DSP-GRP.
     PERFORM     DSP-WR-SEC.
*得意先マスタ表示
     IF  (SYORI  =  2 OR 3)
         PERFORM     DSP-TEN1-SEC
         MOVE       "SCREEN"   TO        DSP-GRP
         PERFORM     DSP-WR-SEC
*        MOVE       "GRP001"   TO        DSP-GRP
*        PERFORM     DSP-WR-SEC.
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
                             MOVE    SPACE   TO  D-KOMOKU  D-R002
                             MOVE   "SCREEN" TO  DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE      TORICD  TOKUCD  TNAME
                                             TKANA   TRYAKU  YUBIN
                                             NYBN1   NYBN2
                                             JYU1    JYU2    TEL
                                             FAX     SIME1   SIME2
                                             SIME3   KUBUN1 KUBUN2
                                             KUBUN3  KUBUN4 KUBUN5
                                             KUBUN6  KUBUN7 KUBUN8
                                             GENKA   BAIKA   R002
                                             FDENNO  FKAINO FENDNO
                                             UDENNO  UKAINO UENDNO
                                             SOKOCD  SOKONM DENCD
                                             ZEIKBN
                             GO        TO   MAIN-020
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
*締日（１）
     IF  (SIME1  =  0 )
         MOVE     "R"        TO   EDIT-OPTION  OF  SIME1
         MOVE     "C"        TO   EDIT-CURSOR  OF  SIME1
         MOVE      MSG08     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
       ELSE
         MOVE     "M"        TO   EDIT-OPTION  OF  SIME1
         MOVE      SPACE     TO   EDIT-CURSOR  OF  SIME1
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-IF.
*伝発区分
     IF  (KUBUN1  =  0 OR 9)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN1
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN1
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN1
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN1
         MOVE      MSG04     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*付番区分
     IF  (KUBUN7  =  0 OR 9)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN7
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN7
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN7
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN7
         MOVE      MSG13     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*2004/06/22 NAV START*
*税計算区分
     IF  (ZEIKBN  =  "0" OR "9")
         MOVE     "M"        TO   EDIT-OPTION  OF  ZEIKBN
         MOVE      SPACE     TO   EDIT-CURSOR  OF  ZEIKBN
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  ZEIKBN
         MOVE     "C"        TO   EDIT-CURSOR  OF  ZEIKBN
         MOVE      MSG16     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*2004/06/22 NAV END  *
*税端数区分
     IF  (KUBUN2  =  0 OR 4 OR 9)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN2
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN2
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN2
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN2
         MOVE      MSG06     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*ＣＨＫＤ区分
*2001/07/31  モジュラース１０（ウエイト１．３）追加の為
     IF  (KUBUN5  =  0 OR 1 OR 2 OR 3 OR 5 OR 6 OR 7)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN5
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN5
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN5
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN5
         MOVE      MSG07     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
*ＣＨＫＤ２区分
*2001/07/31  モジュラース１０（ウエイト１．３）追加の為
     IF  (KUBUN6  =  0 OR 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7)
         MOVE     "M"        TO   EDIT-OPTION  OF  KUBUN6
         MOVE      SPACE     TO   EDIT-CURSOR  OF  KUBUN6
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
       ELSE
         MOVE     "R"        TO   EDIT-OPTION  OF  KUBUN6
         MOVE     "C"        TO   EDIT-CURSOR  OF  KUBUN6
         MOVE      MSG12     TO   MSG1
         MOVE     "MSG1"     TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
         GO   TO   MAIN-030
     END-IF.
******************* 99/09/27追加 *******************************
*倉庫マスタＣＨＫ
     MOVE      SOKOCD         TO   SOK-F01.
     READ      ZSOKMS
       INVALID
               MOVE   SPACE       TO  SOKONM
               MOVE     "R"        TO   EDIT-OPTION  OF  SOKOCD
               MOVE     "C"        TO   EDIT-CURSOR  OF  SOKOCD
               MOVE      MSG14     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
       NOT INVALID
               MOVE  SOK-F02      TO  SOKONM
               MOVE     "M"        TO   EDIT-OPTION  OF  SOKOCD
               MOVE      SPACE     TO   EDIT-CURSOR  OF  SOKOCD
               MOVE     "GRP001"   TO   DSP-GRP
         PERFORM   DSP-WR-SEC
     END-READ.
****************************************************************
******************* 99/11/02追加 *******************************
*自社指定伝票タイプチェック
     IF        DENCD    NOT    NUMERIC
               MOVE     ZERO       TO   DENCD
     END-IF.
     IF        DENCD  =  0 OR 1 OR 2 OR 3 OR 4
               MOVE     "M"        TO   EDIT-OPTION OF DENCD
               MOVE     SPACE      TO   EDIT-CURSOR OF DENCD
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
     ELSE
               MOVE     "R"        TO   EDIT-OPTION OF DENCD
               MOVE     "C"        TO   EDIT-CURSOR OF DENCD
               MOVE      MSG15     TO   MSG1
               MOVE     "MSG1"     TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               MOVE     "GRP001"   TO   DSP-GRP
               PERFORM   DSP-WR-SEC
               GO   TO   MAIN-030
     END-IF.
****************************************************************
*
     MOVE     "Y"            TO   R002.
     MOVE     "R002"         TO   DSP-GRP.
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
                             MOVE    SPACE   TO  D-KOMOKU  D-R002
                             MOVE   "SCREEN" TO  DSP-GRP
                             PERFORM DSP-WR-SEC
                             INITIALIZE      TORICD  TOKUCD  TNAME
                                             TKANA   TRYAKU  YUBIN
                                             NYBN1   NYBN2  SOKOCD
                                             SOKONM  DENCD
                                             JYU1    JYU2    TEL
                                             FAX     SIME1   SIME2
                                             SIME3   KUBUN1 KUBUN2
                                             KUBUN3  KUBUN4 KUBUN5
                                             KUBUN6  KUBUN7 KUBUN8
                                             GENKA   BAIKA   R002
                                             UKAINO  UENDNO UDENNO
                                             FKAINO  FENDNO FDENNO
                                             ZEIKBN
                             GO          TO   MAIN-020
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
           MOVE    MSG11    TO   MSG1
           MOVE   "MSG1"    TO   DSP-GRP
           PERFORM DSP-WR-SEC
           GO          TO   MAIN-040
     ELSE
           MOVE    SPACE    TO   MSG1
           MOVE   "MSG1"    TO   DSP-GRP
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
              MOVE     SYS-DATE   TO  TOK-F98
              WRITE    TOK-REC
              END-WRITE
         WHEN     2
              REWRITE   TOK-REC
              END-REWRITE
         WHEN     3
              DELETE    HTOKMS
              END-DELETE
     END-EVALUATE.
*
*****MOVE     SPACE          TO      D-KOMOKU.
     MOVE     SPACE          TO      D-KOMOKU  D-R002.
     MOVE    "SCREEN"        TO      DSP-GRP.
     PERFORM  DSP-WR-SEC.
     INITIALIZE      TORICD  TOKUCD  TNAME  TKANA   TRYAKU  YUBIN
                     NYBN1   NYBN2   SOKOCD SOKONM  DENCD
                     JYU1    JYU2    TEL    FAX     SIME1   SIME2
                     SIME3   KUBUN1  KUBUN2 KUBUN3  KUBUN4  KUBUN5
                     KUBUN6  KUBUN7  KUBUN8 FKAINO  FENDNO  FDENNO
                     UKAINO  UENDNO  UDENNO GENKA   BAIKA   R002
                     ZEIKBN.
     GO                      TO      MAIN-020.
 MAIN-EXIT.
     EXIT.
****************************************************************
*                   ＥＮＤ処理                                 *
****************************************************************
 END-SEC               SECTION.
     CLOSE             HTOKMS
                       ZSOKMS
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
     MOVE       HEN-DATE     TO  SDATE.
     MOVE       HEN-TIME     TO  STIME.
     MOVE       SPACE        TO  DSP-PRO.
     WRITE      FM011.
 DSP-WR-EXIT.
     EXIT.
****************************************************************
*    初期画面　表示                                          *
****************************************************************
 DSP-INIT-SEC             SECTION.
     MOVE    SPACE           TO   FM011.
     MOVE    SPACE           TO   DSP-AREA.
     MOVE   "FM011"          TO   DSP-FMT.
     MOVE   "SCREEN"         TO   DSP-GRP.
     MOVE   "CL"             TO   DSP-PRO.
     MOVE    HEN-DATE        TO   SDATE.
     MOVE    HEN-TIME        TO   STIME.
     WRITE   FM011.
     INITIALIZE              FM011.
 DSP-INIT-A-EXIT.
     EXIT.
****************************************************************
*            商品Ｍのレコードを画面に転送            *
****************************************************************
 DSP-TEN1-SEC              SECTION.
     MOVE     TOK-F01        TO        TORICD.
     MOVE     TOK-F52        TO        TOKUCD.
     MOVE     TOK-F02        TO        TNAME.
     MOVE     TOK-F04        TO        TKANA.
     MOVE     TOK-F03        TO        TRYAKU.
************* 99/09/27追加 ***************************
     MOVE     TOK-F801       TO        NYBN1.
     MOVE     TOK-F802       TO        NYBN2.
******************************************************
     MOVE     TOK-F05        TO        YUBIN.
     MOVE     TOK-F06        TO        JYU1.
     MOVE     TOK-F07        TO        JYU2.
     MOVE     TOK-F08        TO        TEL.
     MOVE     TOK-F09        TO        FAX.
     MOVE     TOK-F12(1)     TO        SIME1.
     MOVE     TOK-F12(2)     TO        SIME2.
     MOVE     TOK-F12(3)     TO        SIME3.
     MOVE     TOK-F84        TO        KUBUN1.
     MOVE     TOK-F88        TO        KUBUN2.
     MOVE     TOK-F91        TO        KUBUN3.
     MOVE     TOK-F92        TO        KUBUN4.
     MOVE     TOK-F93        TO        KUBUN5.
     MOVE     TOK-F94        TO        KUBUN6.
     MOVE     TOK-F89        TO        KUBUN7.
     MOVE     TOK-F82        TO        KUBUN8.
     MOVE     TOK-F55        TO        GENKA.
     MOVE     TOK-F56        TO        BAIKA.
     MOVE     TOK-F54        TO        UDENNO.
     MOVE     TOK-F57        TO        UKAINO.
     MOVE     TOK-F58        TO        UENDNO.
     MOVE     TOK-F59        TO        FDENNO.
     MOVE     TOK-F60        TO        FKAINO.
     MOVE     TOK-F61        TO        FENDNO.
************* 99/09/27追加 ***************************
     MOVE     TOK-F81        TO        SOKOCD  SOK-F01.
***  倉庫名称取得
     READ      ZSOKMS
       INVALID
               MOVE   SPACE       TO  SOKONM
       NOT INVALID
               MOVE  SOK-F02      TO  SOKONM
     END-READ.
******************************************************
************* 99/11/02追加 ***************************
     MOVE     TOK-F79        TO        DENCD.
************* 04/06/22追加 ***************************
     MOVE     TOK-F97        TO        ZEIKBN.
******************************************************
 DSP-TEN1-EXIT.
     EXIT.
****************************************************************
*            画面より商品Ｍレコードに転送            *
****************************************************************
 DSP-TEN2-SEC           SECTION.
     MOVE     TORICD         TO        TOK-F01.
     MOVE     TOKUCD         TO        TOK-F52.
     MOVE     TNAME          TO        TOK-F02.
     MOVE     TKANA          TO        TOK-F04.
     MOVE     TRYAKU         TO        TOK-F03.
************* 99/09/27追加 ***************************
     MOVE     NYBN1          TO        TOK-F801.
     MOVE     NYBN2          TO        TOK-F802.
******************************************************
     MOVE     YUBIN          TO        TOK-F05.
     MOVE     JYU1           TO        TOK-F06.
     MOVE     JYU2           TO        TOK-F07.
     MOVE     TEL            TO        TOK-F08.
     MOVE     FAX            TO        TOK-F09.
     MOVE     SIME1          TO        TOK-F12(1).
     MOVE     SIME2          TO        TOK-F12(2).
     MOVE     SIME3          TO        TOK-F12(3).
     MOVE     KUBUN1         TO        TOK-F84.
     MOVE     KUBUN2         TO        TOK-F88.
     MOVE     KUBUN3         TO        TOK-F91.
     MOVE     KUBUN4         TO        TOK-F92.
     MOVE     KUBUN5         TO        TOK-F93.
     MOVE     KUBUN6         TO        TOK-F94.
     MOVE     KUBUN7         TO        TOK-F89.
     MOVE     KUBUN8         TO        TOK-F82.
     MOVE     GENKA          TO        TOK-F55.
     MOVE     BAIKA          TO        TOK-F56.
     MOVE     UDENNO         TO        TOK-F54.
     MOVE     UKAINO         TO        TOK-F57.
     MOVE     UENDNO         TO        TOK-F58.
     MOVE     FDENNO         TO        TOK-F59.
     MOVE     FKAINO         TO        TOK-F60.
     MOVE     FENDNO         TO        TOK-F61.
************* 99/09/27追加 ***************************
     MOVE     SOKOCD         TO        TOK-F81.
******************************************************
************* 99/11/02追加 ***************************
     MOVE     DENCD          TO        TOK-F79.
************* 04/06/22追加 ***************************
     MOVE     ZEIKBN         TO        TOK-F97.
******************************************************
     MOVE     SYS-DATE       TO        TOK-F99.
 DSP-TEN2-EXIT.
     EXIT.
