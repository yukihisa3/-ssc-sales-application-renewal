****************************************************************
*    IDENTIFICATION     DIVISION                               *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            OMNT050.
 AUTHOR.                T.A.
 DATE-WRITTEN.          90/09/20.
****************************************************************
*    ENVIRONMENT        DIVISION                               *
****************************************************************
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         STATION    IS   STA.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 条件 Ｆ >>-------------*
     SELECT   HJYOKY    ASSIGN    DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01  JYO-F02
                        FILE      STATUS    JYO-ST.
*----<< ﾋｮｳｼﾞ ﾌｧｲﾙ >>-*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
****************************************************************
*    DATA               DIVISION                               *
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*----<< ｼﾞｮｳｹﾝ ﾌｧｲﾙ >>-*
 FD  HJYOKY
                   BLOCK     CONTAINS 8    RECORDS.
     COPY     HJYOKEN    OF  XFDLIB
     JOINING  JYO        AS  PREFIX.
*----<< ﾋｮｳｼﾞ  ﾌｧｲﾙ >>-*
 FD  DSPFILE
                   LABEL     RECORD    STANDARD.
     COPY     FM050A     OF  XMDLIB.
     COPY     FM050B     OF  XMDLIB.
*--------------------------------------------------------------*
*    WORKING-STORAGE    SECTION                                *
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
 01  SYORI-F            PIC   9(01).
 01  MODE-F             PIC   9(01).
 01  IDX01              PIC   9(03).
 01  JYO-ST             PIC   X(02).
 01  JYO-ST1            PIC   X(04).
 01  ERR-F              PIC   9(02).
 01  IN-DATA            PIC   X(01).
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
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-ST         PIC  X(02).
 01  WK-DSP-GRP         PIC  X(08).
 01  WK-DSP-PRO         PIC  X(02).
 01  KOUMOKU-MEI.
     03  MEI-DATA.
         05  MEI01   PIC  N(10)     VALUE
             NC"取引区分".
         05  MEI02   PIC  N(10)     VALUE    SPACE.
*
         05  MEI03   PIC  N(10)     VALUE
             NC"倉庫名　　（未使用）".
         05  MEI04   PIC  N(10)     VALUE
             NC"銀行コード（未使用）".
         05  MEI05   PIC  N(10)     VALUE
             NC"支店コード（未使用）".
         05  MEI06   PIC  N(10)     VALUE
             NC"備考名称".
         05  MEI07   PIC  N(10)     VALUE
             NC"商品グループ".
         05  MEI08   PIC  N(10)     VALUE
             NX"4040".
         05  MEI09   PIC  N(10)     VALUE
             NX"4040".
         05  MEI10   PIC  N(10)     VALUE
             NC"専用納品書発行量販店".
         05  MEI11   PIC  N(10)     VALUE
             NC"専用請求書発行量販店".
         05  MEI12   PIC  N(10)     VALUE
             NX"4040".
         05  MEI13   PIC  N(10)     VALUE
             NX"4040".
         05  MEI14   PIC  N(10)     VALUE
             NC"自動発注用係数　　　".
     03  KOUMOKU      REDEFINES  MEI-DATA.
         05  K-MEI    OCCURS   14     PIC  N(10).
*----<<  ﾒｯｾｰｼﾞ ｴﾘｱ >>---------*
 01  MSG-TBL.
     03  MSG-DATA.
         05  FILLER     PIC  N(20)     VALUE
       NX"BEF2B7EFA5D5A5A1A5A4A5EBC5D0CFBFBAD1A4DFA4C7A4B9A1A3".
         05  FILLER     PIC  N(20)     VALUE
       NX"BEF2B7EFA5D5A5A1A5A4A5EBCCA4C5D0CFBFA4C7A4B9A1A3".
         05  FILLER     PIC  N(20)     VALUE
       NX"C6FECECFC3CDA5A8A5E9A1BCA4C7A4B9A1A3".
     03  MSG-DATAR    REDEFINES  MSG-DATA.
         05  MSG      OCCURS    3     PIC  N(20).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ﾒｯｾｰｼﾞ ｴﾘｱ >>-*
 01  DSP-ERR900         PIC  N(14)     VALUE
     NX"C6FECECFA5C7A1BCA5BFB0DBBEEFA4C7A4B9A1A3".
 01  FILE-ERR010        PIC  N(15)     VALUE
     NX"BEF2B7EFA5D5A5A1A5A4A5EB4040A5AAA1BCA5D0A1BCA1AAA1AA".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 PROCEDURE              DIVISION.
*--------------------------------------------------------------*
*    LEVEL 0        ERROR   SHORI                              *
*--------------------------------------------------------------*
 DECLARATIVES.
 000-JYO-ERR           SECTION.
     USE AFTER     EXCEPTION      PROCEDURE HJYOKY.
     DISPLAY  JYO-ST              UPON STA.
**** DISPLAY  JYO-ST1             UPON STA.
     DISPLAY  FILE-ERR010         UPON STA.
     ACCEPT   IN-DATA             FROM STA.
     STOP  RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-SEC          SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL     SYORI-F = 9.
     PERFORM  END-SEC.
     STOP RUN.
 PROG-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-SEC           SECTION.
     OPEN     I-O   HJYOKY   DSPFILE.
     MOVE     1              TO   SYORI-F.
     MOVE     ZERO           TO   MODE-F.
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
 INIT-SEC-EXIT.
      EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.
 MAIN-010.
     PERFORM    MED1-SEC    UNTIL  SYORI-F  NOT =  1.
     IF  SYORI-F   =    9
              GO   TO   MAIN-SEC-EXIT.
 MAIN-030.
     PERFORM    MED2-SEC     UNTIL  SYORI-F  NOT =  2.
     IF  SYORI-F   =    9
              MOVE      1         TO   SYORI-F
              GO   TO   MAIN-010.
 MAIN-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-SEC            SECTION.
     CLOSE    HJYOKY.
     CLOSE    DSPFILE.
 END-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ｺｰﾄﾞ-INPUT                                   *
*--------------------------------------------------------------*
 MED1-SEC          SECTION.
     PERFORM  DSP1-INIT.
     IF  MODE-F    =    ZERO
              MOVE     1              TO   MODE-F.
 MED1-010.
     EVALUATE    MODE-F
        WHEN      1
                  MOVE  NX"C4C9B2C3"   TO   WMODE
        WHEN      2
                  MOVE  NX"BDA4C0B5"   TO   WMODE
        WHEN      3
                  MOVE  NX"BAEFBDFC"   TO   WMODE
     END-EVALUATE.
     MOVE  "WMODE"        TO   DSP-GRP.
     PERFORM    DSP1-WRITE.
     MOVE     ZERO           TO   IDX01.
     MOVE     "GP01"         TO   DSP-GRP.
     MOVE     "NE"           TO   DSP-PRO.
     PERFORM  DSP-READ.
     EVALUATE    DSP-FNC
        WHEN      "F001"
                  MOVE  1              TO   MODE-F
                  GO    TO   MED1-010
        WHEN      "F002"
                  MOVE  2              TO   MODE-F
                  GO    TO   MED1-010
        WHEN      "F003"
                  MOVE  3              TO   MODE-F
                  GO    TO   MED1-010
        WHEN      "F004"
                  GO  TO     MED1-010
        WHEN      "F005"
                  MOVE       9     TO  SYORI-F
                  GO  TO     MED1-SEC-EXIT
        WHEN      "E000"
                  CONTINUE
        WHEN      OTHER
                  GO TO      MED1-010
     END-EVALUATE.
     IF    R01A    =     ZERO
           GO      TO    MED1-010.
     MOVE  R01A     TO   IDX01.
 MED1-030.
     MOVE     "GPKAKU"       TO   DSP-GRP.
     MOVE     "NE"           TO   DSP-PRO.
     PERFORM  DSP-READ.
     EVALUATE    DSP-FNC
        WHEN      "F004"
                  GO  TO     MED1-010
        WHEN      "F005"
                  MOVE  9    TO   SYORI-F
                  GO  TO     MED1-SEC-EXIT
        WHEN      "E000"
                  CONTINUE
        WHEN      OTHER
                  GO TO      MED1-030
     END-EVALUATE.
*
     MOVE  2        TO   SYORI-F.
 MED1-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2   ｺｳﾓｸ -INPUT                                    *
*--------------------------------------------------------------*
 MED2-SEC         SECTION.
     PERFORM  DSP2-INIT.
 MED2-010.
     EVALUATE    MODE-F
        WHEN      1
                  MOVE  NX"C4C9B2C3"   TO   W01C
        WHEN      2
                  MOVE  NX"BDA4C0B5"   TO   W01C
        WHEN      3
                  MOVE  NX"BAEFBDFC"   TO   W01C
     END-EVALUATE.
     IF  IDX01     <    1    OR   >    14
        THEN
        MOVE     SPACE          TO   W02C
        ELSE
        MOVE     K-MEI (IDX01)  TO   W02C
     END-IF.
     MOVE     "GP01"         TO   DSP-GRP.
     PERFORM  DSP2-WRITE.
 MED2-030.
     MOVE     "GP02"         TO   DSP-GRP.
     PERFORM  DSP-READ.
     EVALUATE    DSP-FNC
        WHEN      "F004"
                  GO  TO     MED2-030
        WHEN      "F005"
                  MOVE  9    TO   SYORI-F
                  GO  TO     MED2-SEC-EXIT
        WHEN      "E000"
                  CONTINUE
        WHEN      OTHER
                  GO TO      MED2-030
     END-EVALUATE.
*
     MOVE     ZERO      TO   ERR-F.
     PERFORM       KEY2-CHK.
     IF  ERR-F     NOT  =    0
              GO   TO   MED2-030.
 MED2-050.
     IF  MODE-F    =    1   OR   2
         PERFORM        DATAIN-SEC.
     EVALUATE   SYORI-F
         WHEN   6
                MOVE    2    TO   SYORI-F
                GO  TO     MED2-SEC-EXIT
         WHEN   9
                GO  TO     MED2-SEC-EXIT
     END-EVALUATE.
     PERFORM    KAKUNIN-SEC.
     EVALUATE   SYORI-F
         WHEN   6
                MOVE    2    TO   SYORI-F
                GO  TO     MED2-SEC-EXIT
         WHEN   9
                GO  TO     MED2-SEC-EXIT
     END-EVALUATE.
     MOVE     2    TO   SYORI-F.
 MED2-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3   KEY2 - CHK                                     *
*--------------------------------------------------------------*
 KEY2-CHK         SECTION.
     IF  MODE-F    =    1
         MOVE      IDX01     TO   JYO-F01
         MOVE      R03C      TO   JYO-F02
         READ      HJYOKY     INVALID
                                  PERFORM   ERR-CL
                         NOT  INVALID
                                  MOVE  1  TO  ERR-F
                                  PERFORM   ERR-SEC
         END-READ
     END-IF.
     IF  MODE-F    =    2    OR   3
         MOVE      IDX01     TO   JYO-F01
         MOVE      R03C      TO   JYO-F02
         READ      HJYOKY     INVALID
                                  MOVE  2  TO  ERR-F
                                  PERFORM      ERR-SEC
                             NOT  INVALID
                                  PERFORM      ERR-CL
                                  PERFORM      BODY-DSP
         END-READ
     END-IF.
 KEY2-CHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     BODY  DSP                                    *
*--------------------------------------------------------------*
 BODY-DSP         SECTION.
     MOVE     JYO-F03          TO       K03.
     MOVE     JYO-F04          TO       K04.
     MOVE     JYO-F05          TO       K05.
     MOVE     JYO-F06          TO       K06.
     MOVE     JYO-F07          TO       K07.
     MOVE     JYO-F08          TO       K08.
     MOVE     JYO-F09          TO       K09.
     MOVE     JYO-F10          TO       K10.
     MOVE     JYO-F11          TO       K11.
     MOVE     JYO-F12          TO       K12.
     MOVE     JYO-F12A         TO       K12A.
     MOVE     JYO-F12B         TO       K12B.
     MOVE     JYO-F12C         TO       K12C.
     MOVE     JYO-F13          TO       K13.
     MOVE     JYO-F14          TO       K14.
     MOVE     JYO-F15          TO       K14A.
     MOVE     "GP03"         TO       DSP-GRP.
     PERFORM  DSP2-WRITE.
 BODY-SDP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     DATA-INPUT                                   *
*--------------------------------------------------------------*
 DATAIN-SEC           SECTION.
     MOVE     "GP03"         TO   DSP-GRP.
     PERFORM  DSP-READ.
     EVALUATE    DSP-FNC
        WHEN      "F004"
                  MOVE  6    TO   SYORI-F
                  GO  TO     DATAIN-EXIT
        WHEN      "F005"
                  MOVE  9    TO   SYORI-F
                  GO  TO     DATAIN-EXIT
        WHEN      "E000"
                  CONTINUE
        WHEN      OTHER
                  GO TO      DATAIN-SEC
     END-EVALUATE.
 DATAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     KAKUNIN-INPUT                                *
*--------------------------------------------------------------*
 KAKUNIN-SEC          SECTION.
     MOVE     "GPKAKU"       TO   DSP-GRP.
     PERFORM  DSP-READ.
     EVALUATE    DSP-FNC
        WHEN      "F004"
                  MOVE  6    TO   SYORI-F
                  GO  TO     KAKUNIN-EXIT
        WHEN      "F005"
                  MOVE  9    TO   SYORI-F
                  GO  TO     KAKUNIN-EXIT
        WHEN      "E000"
                  CONTINUE
        WHEN      OTHER
                  GO TO      KAKUNIN-SEC
     END-EVALUATE.
     IF    MODE-F = 1
           PERFORM  JYO-WRITE.
     IF    MODE-F = 2
           PERFORM  JYO-REWRITE.
     IF    MODE-F = 3
           DELETE   HJYOKY.
 KAKUNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4   MED TO HJYOKY  ｼｮﾘ                             *
*--------------------------------------------------------------*
 JYO-REWRITE         SECTION.
     MOVE     K03            TO   JYO-F03.
     MOVE     K04            TO   JYO-F04.
     MOVE     K05            TO   JYO-F05.
     MOVE     K06            TO   JYO-F06.
     MOVE     K07            TO   JYO-F07.
     MOVE     K08            TO   JYO-F08.
     MOVE     K09            TO   JYO-F09.
     MOVE     K10            TO   JYO-F10.
     MOVE     K11            TO   JYO-F11.
     MOVE     K12            TO   JYO-F12.
     MOVE     K12A           TO   JYO-F12A.
     MOVE     K12B           TO   JYO-F12B.
     MOVE     K12C           TO   JYO-F12C.
     MOVE     K13            TO   JYO-F13.
     MOVE     K14            TO   JYO-F14.
     MOVE     K14A           TO   JYO-F15.
     REWRITE  JYO-REC.
 REWRITE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4   JYO-F WRITE  ｼｮﾘ                                 *
*--------------------------------------------------------------*
 JYO-WRITE       SECTION.
     MOVE     SPACE          TO   JYO-REC.
     INITIALIZE  JYO-REC.
     MOVE     IDX01          TO   JYO-F01.
     MOVE     R03C           TO   JYO-F02.
     MOVE     K03            TO   JYO-F03.
     MOVE     K04            TO   JYO-F04.
     MOVE     K05            TO   JYO-F05.
     MOVE     K06            TO   JYO-F06.
     MOVE     K07            TO   JYO-F07.
     MOVE     K08            TO   JYO-F08.
     MOVE     K09            TO   JYO-F09.
     MOVE     K10            TO   JYO-F10.
     MOVE     K11            TO   JYO-F11.
     MOVE     K12            TO   JYO-F12.
     MOVE     K12A           TO   JYO-F12A.
     MOVE     K12B           TO   JYO-F12B.
     MOVE     K12C           TO   JYO-F12C.
     MOVE     K13            TO   JYO-F13.
     MOVE     K14            TO   JYO-F14.
     MOVE     K14A           TO   JYO-F15.
     WRITE    JYO-REC  INVALID
              DISPLAY  FILE-ERR010      UPON STA
              ACCEPT   IN-DATA          FROM STA
              STOP     RUN.
 JYO-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ1 ｼｮｷ ﾋｮｩｼﾞ                         *
*--------------------------------------------------------------*
 DSP1-INIT          SECTION.
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     LOW-VALUE      TO   FM050A.
     MOVE     "FM050A"       TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     MOVE     "CL"           TO   DSP-PRO.
     MOVE     HEN-DATE       TO   SDATE1.
     MOVE     HEN-TIME       TO   STIME2.
     WRITE    FM050A.
     MOVE     MEI01     TO   W01A.
     MOVE     MEI02     TO   W02A.
     MOVE     MEI03     TO   W03A.
     MOVE     MEI04     TO   W04A.
     MOVE     MEI05     TO   W05A.
     MOVE     MEI06     TO   W06A.
     MOVE     MEI07     TO   W07A.
     MOVE     MEI08     TO   W08A.
     MOVE     MEI09     TO   W09A.
     MOVE     MEI10     TO   W10A.
     MOVE     MEI11     TO   W11A.
     MOVE     MEI12     TO   W12A.
     MOVE     MEI13     TO   W13A.
     MOVE     MEI14     TO   W14A.
     MOVE     "GP03"    TO   DSP-GRP.
     PERFORM  DSP1-WRITE.
     MOVE     ZERO      TO   R01A.
 DSP1-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ2 ｼｮｷ ﾋｮｩｼﾞ                         *
*--------------------------------------------------------------*
 DSP2-INIT          SECTION.
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     LOW-VALUE      TO   FM050B.
     MOVE     "FM050B"       TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     MOVE     "CL"           TO   DSP-PRO.
     MOVE     HEN-DATE       TO   SDATE2.
     MOVE     HEN-TIME       TO   STIME2.
     WRITE    FM050B.
     MOVE     SPACE     TO   W01C W02C R03C.
     MOVE     SPACE     TO   K03  K13  K14  K14A.
     MOVE     ZERO      TO   K04  K05  K06  K07
                        K08  K09  K10  K11  K12  K12A  K12B  K12C.
 DSP2-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 DSP-READ           SECTION.
     MOVE     "NE"           TO   DSP-PRO.
     READ     DSPFILE        AT   END       STOP RUN.
 DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ1 WRITE                             *
*--------------------------------------------------------------*
 DSP1-WRITE         SECTION.
     MOVE     "  "           TO   DSP-PRO.
     WRITE    FM050A.
     IF       DSP-ST    NOT  =    ZERO
              GO   TO   DSP1-WRITE.
 DSP1-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ2 WRITE                             *
*--------------------------------------------------------------*
 DSP2-WRITE         SECTION.
     MOVE     "  "           TO   DSP-PRO.
     WRITE    FM050B.
 DSP2-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  ERR  ﾋｮｩｼﾞ                        *
*--------------------------------------------------------------*
 ERR-SEC           SECTION.
     MOVE     MSG (ERR-F)    TO   ERR01.
     MOVE     "GPERR"        TO   DSP-GRP.
     PERFORM  DSP2-WRITE.
 ERR-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  ERR  ﾋｮｩｼﾞ ｸﾘｱ                    *
*--------------------------------------------------------------*
 ERR-CL           SECTION.
     MOVE     SPACE          TO   ERR01.
     MOVE     "GPERR"        TO   DSP-GRP.
     PERFORM  DSP2-WRITE.
 ERR-CL-EXIT.
     EXIT.
