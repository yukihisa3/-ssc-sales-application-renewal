****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　                                  *
*    業務名　　　　　　　：　内部統制対応　　　　　　　        *
*    モジュール名　　　　：　担当者マスタ保守　                *
*    作成日／作成者　　　：　08/07/28 NAV.                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
*    処理概要　　　　　　：　担当者マスタのメンテを行う。　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SMNT080.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*---<<  担当者マスタ  >>---*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE      STATUS    IS   TAN-STATUS.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYK-F01
                                                 JYK-F02
                        FILE      STATUS    IS   JYK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FMNT080   OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  担当者マスタ  >>---*
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*---<<  条件マスタ  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYK       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  TAN-STATUS          PIC  X(02).
     02  JYK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  FG-HTANMS-INV         PIC  9(01)  VALUE ZERO.
     02  FG-HJYOKEN22-INV      PIC  9(01).
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
     02  WK-INI-BUMNCD       PIC  X(04).
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
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_再入力".
     02  PMSG03            PIC N(20) VALUE
                           NC"_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SMNT080".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"既に登録済です。".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"担当者コードが未入力です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"部門コードが未入力です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"処理区分が違います".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"　　　".
     02  MSG-ERR8            PIC  N(28)  VALUE
            NC"担当者コードが未登録です。".
     02  MSG-ERR9            PIC  N(28)  VALUE
            NC"部門コードが未登録です。".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"該当項目に誤りがあります。".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"担当者名が未入力です。".
     02  MSG-ERR12           PIC  N(28)  VALUE
            NC"権限レベルが未入力です。".
     02  MSG-ERR13           PIC  N(28)  VALUE
            NC"権限が未入力です。".
     02  MSG-ERR14           PIC  N(28)  VALUE
            NC"担当者名に誤りがあります。".
     02  MSG-ERR15           PIC  N(28)  VALUE
            NC"権限レベルに誤りがあります。".
     02  MSG-ERR16           PIC  N(28)  VALUE
            NC"権限に誤りがあります。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  16   TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).

****  リンク領域  ***
 LINKAGE               SECTION.
 01  PARA-AREA.
**   担当者コード
     03  PARA-TANCD         PIC  X(02).
     03  PARA-INI-BUMNCD    PIC  X(04).

*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING PARA-AREA.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTANMS.
     MOVE     "HTANMS"       TO   ERR-FL-ID.
     MOVE     TAN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.

**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*    1.0  初期処理                                             *
****************************************************************
 INIT-SEC                    SECTION.
*    システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM  DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
*    画面表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
*    システム日付取得
     ACCEPT  WK-TIME  FROM TIME.
*    画面表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.
*
     OPEN  I-O    DSPF.
     OPEN  I-O    HTANMS.
     OPEN  INPUT  HJYOKEN.

     MOVE  "FMNT080"        TO  DSP-FORMAT.
     MOVE  SPACE            TO  DSP-FMNT080.
     MOVE  SPACE            TO  END-FLG.
     MOVE  "1"              TO  MAIN-FLG.
     MOVE  2                TO  WK-SYORI.
     MOVE  SPACE            TO  DSP-PROC.
 INIT-END.
     EXIT.
****************************************************************
*    2.0  メイン処理                                           *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE  MAIN-FLG
       WHEN  "1"  PERFORM  SYORI-SUB
       WHEN  "2"  PERFORM  HEAD-SUB
       WHEN  "3"  PERFORM  BODY-SUB
       WHEN  "4"  PERFORM  KAKUNIN-SUB
       WHEN  "5"  PERFORM  FILPRT-SUB
       WHEN  OTHER  CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*    処理区分入力                                              *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG03           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "SYORI"          TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**
** 処理区分入力
**
     MOVE  ZERO             TO  ERR-MSG-CD.
     EVALUATE  DSP-FUNC
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

       WHEN  "E000"
         PERFORM  SYORICHK-SUB
         IF ERR-MSG-CD = ZERO
            MOVE  "2"       TO  MAIN-FLG
            PERFORM  HEADDEL-SUB

            MOVE  PARA-INI-BUMNCD  TO  DSP-BUMNCD
            PERFORM  BUMON-GET-SUB
            IF FG-HJYOKEN22-INV = 1
               MOVE  SPACE         TO  DSP-BUMNNM
            ELSE
               MOVE  JYK-F03       TO  DSP-BUMNNM
            END-IF

            MOVE  "C"       TO  EDIT-CURSOR  OF  DSP-TANCD
         END-IF

       WHEN  OTHER
         MOVE 01            TO  ERR-MSG-CD

     END-EVALUATE.
*
 SYORI-END.
     EXIT.
*--------------------------------------------------------------*
*    画面表示処理                                              *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE  "GPALL"          TO  DSP-GROUP.
     MOVE  SPACE            TO  DSP-PROC.
     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME         TO  DSP-STIME.
     WRITE  DSP-FMNT080.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*    エラーメッセージセット                                    *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
*    エラー メッセージ セット
     IF ERR-MSG-CD = ZERO
        MOVE  SPACE         TO  DSP-MSG1
     ELSE
        MOVE  ERR-MSG(ERR-MSG-CD)  TO  DSP-MSG1
        MOVE  ZERO                 TO  ERR-MSG-CD
     END-IF.

 MSG-END.
     EXIT.
*--------------------------------------------------------------*
*    画面データの入力処理                                      *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"             TO  DSP-PROC.
     READ  DSPF.

 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*    処理区分の入力チェック                                    *
*--------------------------------------------------------------*
 SYORICHK-SUB            SECTION.
*    処理区分 CHK
     IF ( DSP-SYORI  NOT  NUMERIC   )
        MOVE  05            TO  ERR-MSG-CD
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SYORI
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-SYORI
     ELSE
        IF ( DSP-SYORI = 1 OR 2 OR 3 )
           IF DSP-SYORI = 1
              MOVE  1       TO  WK-SYORI
           END-IF
           IF DSP-SYORI = 2
              MOVE  2       TO  WK-SYORI
           END-IF
           IF DSP-SYORI = 3
              MOVE  3       TO  WK-SYORI
           END-IF
        ELSE
           MOVE  05         TO  ERR-MSG-CD
           MOVE  "R"        TO  EDIT-OPTION  OF  DSP-SYORI
           MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-SYORI
        END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部入力                                            *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG01           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "GPHEAD"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**
** ヘッド部入力
**
     MOVE  ZERO             TO  ERR-MSG-CD.

*    アテンション判定
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD

       WHEN  "E000"
         PERFORM  HEADCHK-SUB
         IF ERR-MSG-CD = ZERO
            IF WK-SYORI = 3
               MOVE  "4"    TO  MAIN-FLG
            ELSE
               MOVE  "3"    TO  MAIN-FLG
            END-IF

         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*    部門名称取得                                              *
*--------------------------------------------------------------*
 BUMON-GET-SUB          SECTION.
     MOVE  22               TO  JYK-F01.
     MOVE  DSP-BUMNCD       TO  JYK-F02.

     READ  HJYOKEN
       INVALID
         MOVE  1            TO  FG-HJYOKEN22-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HJYOKEN22-INV
     END-READ.

 BUMON-GET-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部消去                                            *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
     MOVE  SPACE            TO  DSP-BUMNCD.
     MOVE  SPACE            TO  DSP-BUMNNM.
     MOVE  SPACE            TO  DSP-TANCD.

     MOVE  "D"              TO  EDIT-OPTION OF DSP-BUMNCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BUMNCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANCD.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部の入力チェック                                  *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BUMNCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BUMNCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANCD.

     IF DSP-BUMNCD = SPACE
        MOVE  PARA-INI-BUMNCD  TO  DSP-BUMNCD
        PERFORM  BUMON-GET-SUB
        IF FG-HJYOKEN22-INV = 1
           MOVE  SPACE      TO  DSP-BUMNNM
        ELSE
           MOVE  JYK-F03    TO  DSP-BUMNNM
        END-IF
     END-IF.

**   IF DSP-BUMNCD = SPACE
**      MOVE  "R"           TO  EDIT-OPTION OF DSP-BUMNCD
**      IF ERR-MSG-CD = ZERO
**         MOVE  04         TO  ERR-MSG-CD
**         MOVE  "C"        TO  EDIT-CURSOR OF DSP-BUMNCD
**      END-IF
**      GO TO  HEADCHK-BUMNCD-EXT
**   END-IF.

     PERFORM  BUMON-GET-SUB.

     MOVE  SPACE            TO  DSP-BUMNNM.
     IF FG-HJYOKEN22-INV = 1
        MOVE  "R"           TO  EDIT-OPTION OF DSP-BUMNCD
        IF ERR-MSG-CD = ZERO
           MOVE  09         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-BUMNCD
        END-IF
     ELSE
        MOVE  JYK-F03       TO  DSP-BUMNNM
     END-IF.

 HEADCHK-BUMNCD-EXT.

     IF DSP-TANCD = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TANCD
        IF ERR-MSG-CD = ZERO
           MOVE  03         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TANCD
        END-IF
        GO TO  HEADCHK-TANCD-EXT
     END-IF.

 HEADCHK-TANCD-EXT.

     IF ERR-MSG-CD NOT = ZERO
        GO TO  HEADCHK-END
     END-IF.

     MOVE DSP-BUMNCD        TO  TAN-F01.
     MOVE DSP-TANCD         TO  TAN-F02.

     READ HTANMS
       INVALID   KEY
         MOVE  1            TO  FG-HTANMS-INV
       NOT INVALID KEY
         MOVE  ZERO         TO  FG-HTANMS-INV
     END-READ.

     IF FG-HTANMS-INV = 1
        IF WK-SYORI NOT = 1
           MOVE  "R"        TO  EDIT-OPTION OF DSP-TANCD
           IF ERR-MSG-CD = ZERO
              MOVE  08      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-TANCD
           END-IF
        END-IF
     ELSE
        IF WK-SYORI = 1
           MOVE  "R"        TO  EDIT-OPTION OF DSP-TANCD
           IF ERR-MSG-CD = ZERO
              MOVE  02      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-TANCD
           END-IF
        END-IF
     END-IF.

     IF ERR-MSG-CD NOT = ZERO
        GO TO  HEADCHK-END
     END-IF.

     PERFORM  FILE-SUB.
*
 HEADCHK-END.
     EXIT.

*--------------------------------------------------------------*
*    ファイルセット                                            *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
     IF WK-SYORI = 1
        MOVE  SPACE              TO  DSP-BODY
     ELSE
        MOVE  SPACE              TO  DSP-BODY
        MOVE  TAN-F03            TO  DSP-TANNM
        MOVE  TAN-F04            TO  DSP-TANNMK
        MOVE  TAN-F05            TO  DSP-KENGNL
        MOVE  TAN-F06            TO  DSP-KENGN1

        IF TAN-F07 NOT = ZERO
           MOVE  TAN-F07         TO  DSP-KENGN2
        END-IF

        IF TAN-F08 NOT = ZERO
           MOVE  TAN-F08         TO  DSP-KENGN3
        END-IF

        IF TAN-F09 NOT = ZERO
           MOVE  TAN-F09         TO  DSP-KENGN4
        END-IF

        IF TAN-F10 NOT = ZERO
           MOVE  TAN-F10         TO  DSP-KENGN5
        END-IF

     END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部入力                                            *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG02           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "GPBODY"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**
**  ボディー部入力
**
     MOVE  ZERO             TO  ERR-MSG-CD.

     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         PERFORM  BODYDEL-SUB
         MOVE  PARA-INI-BUMNCD  TO  DSP-BUMNCD
         PERFORM  BUMON-GET-SUB
         IF FG-HJYOKEN22-INV = 1
            MOVE  SPACE     TO  DSP-BUMNNM
         ELSE
            MOVE  JYK-F03   TO  DSP-BUMNNM
         END-IF

         MOVE  "C"          TO  EDIT-CURSOR OF DSP-TANCD

         MOVE  "2"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD

       WHEN  "F009"
         MOVE   "2"         TO  MAIN-FLG
         MOVE   ZERO        TO  ERR-MSG-CD

       WHEN  "E000"
         PERFORM  BODYCHK-SUB
         IF ERR-MSG-CD = ZERO
            MOVE  "4"       TO  MAIN-FLG
         END-IF

       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部消去                                            *
*--------------------------------------------------------------*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE            TO  DSP-BODY.

     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANNM.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANNM.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANNMK.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANNMK.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGNL.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGNL.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN4.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN5.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ入力チェック                                      *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.

     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANNM.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANNM.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANNMK.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANNMK.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGNL.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGNL.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN4.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENGN5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENGN5.

     IF DSP-TANNM = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TANNM
        IF ERR-MSG-CD = ZERO
           MOVE  11         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TANNM
        END-IF
        GO TO  BODYCHK-TANNM-EXT
     END-IF.

 BODYCHK-TANNM-EXT.

     IF DSP-KENGNL = ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KENGNL
        IF ERR-MSG-CD = ZERO
           MOVE  12         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KENGNL
        END-IF
        GO TO  BODYCHK-KENGNL-EXT
     END-IF.

     IF DSP-KENGNL < 1 OR > 5
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KENGNL
        IF ERR-MSG-CD = ZERO
           MOVE  15         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KENGNL
        END-IF
     END-IF.

 BODYCHK-KENGNL-EXT.

     IF DSP-KENGN1 = ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KENGN1
        IF ERR-MSG-CD = ZERO
           MOVE  13         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KENGN1
        END-IF
        GO TO  BODYCHK-KENGN1-EXT
     END-IF.

     IF DSP-KENGN1 < 1 OR > 5
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KENGN1
        IF ERR-MSG-CD = ZERO
           MOVE  16         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KENGN1
        END-IF
     END-IF.

 BODYCHK-KENGN1-EXT.
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*    確認入力                                                  *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF ERR-MSG-CD = ZERO
        MOVE  "Y"           TO  DSP-KAKNIN
     END-IF.

     PERFORM  MSG-SEC.
     IF WK-SYORI = 3
        MOVE  PMSG01        TO  DSP-MSG2
     ELSE
        MOVE  PMSG02        TO  DSP-MSG2
     END-IF.

     PERFORM  DSP-WRITE-SUB.
     MOVE  "GPKAKU"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
     MOVE  ZERO             TO  ERR-MSG-CD.
 KAKUNIN.
**
** 確認
**
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
         MOVE  SPACE        TO  DSP-KAKNIN

       WHEN  "F009"
         IF WK-SYORI = 3
            MOVE  01        TO  ERR-MSG-CD
         ELSE
            MOVE  "3"       TO  MAIN-FLG
         END-IF
         MOVE  SPACE        TO  DSP-KAKNIN

       WHEN  "E000"
         IF DSP-KAKNIN NOT = "Y"
            MOVE  06        TO  ERR-MSG-CD
         ELSE
            MOVE  SPACE     TO  DSP-KAKNIN
            MOVE  "5"       TO  MAIN-FLG
         END-IF

       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD

     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*    ファイル更新                                              *
*--------------------------------------------------------------*
 FILPRT-SUB             SECTION.
     IF WK-SYORI =  1
        MOVE  SPACE         TO  TAN-REC
        INITIALIZE          TAN-REC
     END-IF.

     MOVE  DSP-BUMNCD       TO  TAN-F01.
     MOVE  DSP-TANCD        TO  TAN-F02.
     MOVE  DSP-TANNM        TO  TAN-F03.
     MOVE  DSP-TANNMK       TO  TAN-F04.
     MOVE  DSP-KENGNL       TO  TAN-F05.
     MOVE  DSP-KENGN1       TO  TAN-F06.
     MOVE  DSP-KENGN2       TO  TAN-F07.
     MOVE  DSP-KENGN3       TO  TAN-F08.
     MOVE  DSP-KENGN4       TO  TAN-F09.
     MOVE  DSP-KENGN5       TO  TAN-F10.
     IF WK-SYORI = 1
        MOVE  PARA-TANCD    TO  TAN-F94
        MOVE  SYS-DATE      TO  TAN-F95
        MOVE  TIME-AREA(1:6)
                            TO  TAN-F96
     END-IF.
     MOVE  PARA-TANCD       TO  TAN-F97.
     MOVE  SYS-DATE         TO  TAN-F98.
     MOVE  TIME-AREA(1:6)   TO  TAN-F99.
*    処理モードにより追加・更新・削除
     EVALUATE  WK-SYORI
       WHEN  1
         WRITE  TAN-REC    END-WRITE
       WHEN  2
         REWRITE  TAN-REC  END-REWRITE
       WHEN  3
         DELETE  HTANMS    END-DELETE
     END-EVALUATE.
*
     PERFORM  HEADDEL-SUB.
     PERFORM  BODYDEL-SUB.
     MOVE  "1"              TO  MAIN-FLG.

 FILPRT-END.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
     CLOSE  DSPF.
     CLOSE  HTANMS.
     CLOSE  HJYOKEN.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
