# SSY3792I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3792I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩシステム　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ新ＥＤＩシステム　　        *
*    モジュール名　　　　：　ナフコ商品マスタ保守              *
*    作成日／作成者　　　：　2011/11/29 NAV                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
*    処理概要　　　　　　：　ナフコ商品マスタのメンテを行なう。*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3792I.
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
*---<<  ナフコ商品マスタ  >>---*
     SELECT   NFSHOMS    ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                        FILE      STATUS    IS   SHO-STATUS.
*---<<  商品変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  担当者マスタ  >>---*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE      STATUS    IS   TAN-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FSY37921  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  ナフコ商品マスタ  >>---*
 FD  NFSHOMS.
     COPY     NFSHOMS    OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*---<<  商品名称マスタ　  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  担当者マスタ　　  >>---*
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
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
     02  SHO-STATUS          PIC  X(02).
     02  TBL-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TAN-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  NFSHOMS-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTANMS-INV-FLG      PIC  X(03)  VALUE SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY3792I".
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
            NC"サカタ商品ＣＤを入力して下さい。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"商品名称マスタに未登録です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"ＪＡＮＣＤを入力して下さい。".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"原価単価を入力して下さい。".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"売価単価を入力して下さい。".
     02  MSG-ERR8            PIC  N(28)  VALUE
            NC"原価単価が売価単価を超えています。".
     02  MSG-ERR9            PIC  N(28)  VALUE
            NC"（ランク）原価単価が売価単価を超えています。".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"ナフコ商品ＣＤが未登録です。".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"処理区分が違います。".
     02  MSG-ERR12           PIC  N(28)  VALUE
            NC"商品変換ＴＢＬ未登録です。".
     02  MSG-ERR13           PIC  N(28)  VALUE
            NC"商品名称マスタ未登録です。".
     02  MSG-ERR14           PIC  N(28)  VALUE
            NC"商品名を入力して下さい".
     02  MSG-ERR15           PIC  N(28)  VALUE
            NC"商品名カナを入力して下さい。".
     02  MSG-ERR16           PIC  N(28)  VALUE
            NC"部門を入力して下さい。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  16   TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(200).
*
 01  SEQ                   PIC  9(02).
*
****  リンク領域  ***
 LINKAGE               SECTION.
 01  PARA-BUMONCDI          PIC  X(04).
 01  PARA-TANCDI            PIC  X(08).
*
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING
                                             PARA-BUMONCDI
                                             PARA-TANCDI.
*
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
                     PROCEDURE    NFSHOMS.
     MOVE     "NFSHOMS1"      TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.

**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "SHOTBL1"      TO   ERR-FL-ID.
     MOVE     TBL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "MEIMS1"       TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTANMS.
     MOVE     "TANMS1"       TO   ERR-FL-ID.
     MOVE     TAN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 PROGRAM-START               SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROGRAM-END.
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
*ファイルオープン
     OPEN  I-O    DSPF.
     OPEN  I-O    NFSHOMS.
     OPEN  INPUT  HSHOTBL  HMEIMS  HTANMS.

     MOVE  "FSY37921"        TO  DSP-FORMAT.
     MOVE  SPACE            TO  DSP-FSY37921.
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
     MOVE  "HEAD01"         TO  DSP-GROUP.
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
     MOVE  "SCREEN"         TO  DSP-GROUP.
     MOVE  SPACE            TO  DSP-PROC.
     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME         TO  DSP-STIME.
     WRITE  DSP-FSY37921.
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
        MOVE  11            TO  ERR-MSG-CD
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
           MOVE  11         TO  ERR-MSG-CD
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
     MOVE  "HEAD02"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
** ヘッド部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

*    アテンション判定
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD

       WHEN  "E000"
         PERFORM  HEADCHK-SUB
         IF ERR-MSG-CD = ZERO
            MOVE  "D"       TO  EDIT-OPTION OF DSP-NFSYOC
            MOVE  SPACE     TO  EDIT-CURSOR OF DSP-NFSYOC
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
*    ＨＥＡＤ部消去                                            *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
*
     MOVE  SPACE            TO  DSP-GRPSHO.
     MOVE  SPACE            TO  DSP-NFSYOC.
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYORI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYORI.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-NFSYOC.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NFSYOC.
*
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部の入力チェック                                  *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*ナフコ商品コード入力チェック
     IF DSP-NFSYOC = SPACE
        MOVE  "R"    TO  EDIT-OPTION OF DSP-NFSYOC
        MOVE  04     TO  ERR-MSG-CD
        MOVE  "C"    TO  EDIT-CURSOR OF DSP-NFSYOC
        GO           TO  HEADCHK-END
     END-IF.
*ナフコ商品マスタ索引
     MOVE DSP-NFSYOC     TO  SHO-F01.
     READ NFSHOMS
       INVALID   KEY
         MOVE  "INV"        TO  NFSHOMS-INV-FLG
       NOT INVALID KEY
         MOVE  SPACE        TO  NFSHOMS-INV-FLG
     END-READ.
*判定
     IF NFSHOMS-INV-FLG = "INV"   *>ファイルに未存在の時
        IF WK-SYORI NOT = 1       *>登録以外の時、エラー
           MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
           IF ERR-MSG-CD = ZERO
              MOVE  10      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
           END-IF
        END-IF
     ELSE
        IF WK-SYORI = 1 *>ファイルが存在し、登録の場合、エラー
           MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
           IF ERR-MSG-CD = ZERO
              MOVE  02      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
           END-IF
        END-IF
     END-IF.
*エラーがあった場合は、エラーメッセージ出力
     IF ERR-MSG-CD NOT = ZERO
        GO TO  HEADCHK-END
     END-IF.
*項目セット
     PERFORM  FILE-SUB.
*
 HEADCHK-END.
     EXIT.

*--------------------------------------------------------------*
*    ファイルセット                                            *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
     IF WK-SYORI = 1    *>登録の時
        MOVE  SPACE              TO  DSP-MEISAI
********ランク原価／売価初期化
        MOVE  ZERO               TO  DSP-GENTAN DSP-URITAN
        MOVE  ZERO               TO  DSP-GENKA1 DSP-BAIKA1
        MOVE  ZERO               TO  DSP-GENKA2 DSP-BAIKA2
        MOVE  ZERO               TO  DSP-GENKA3 DSP-BAIKA3
        MOVE  ZERO               TO  DSP-GENKA4 DSP-BAIKA4
        MOVE  ZERO               TO  DSP-GENKA5 DSP-BAIKA5
        MOVE  ZERO               TO  DSP-IRESU
        MOVE  SPACE              TO  DSP-BUMON
********登録時、マスタを索引して初期情報をセットする。
********商品変換テーブル索引（１３７６０７＋ナフコ商品ＣＤ）
        MOVE  137607             TO  TBL-F01
        MOVE  DSP-NFSYOC         TO  TBL-F02
        PERFORM HSHOTBL-READ-SEC
        IF  HSHOTBL-INV-FLG = "INV"
            MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
            IF ERR-MSG-CD = ZERO
               MOVE  12      TO  ERR-MSG-CD
               MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
            END-IF
        ELSE
************商品変換テーブル情報をセット
            MOVE TBL-F031    TO  DSP-SHOCD   MEI-F011
            MOVE TBL-F0321   TO  DSP-TAN1    MEI-F0121
            MOVE TBL-F0322   TO  DSP-TAN2    MEI-F0122
            MOVE TBL-F0323   TO  DSP-TAN3    MEI-F0123
            MOVE TBL-F05     TO  DSP-GENTAN
            MOVE TBL-F06     TO  DSP-URITAN
************商品名取得の為、商品名称マスタ索引
            PERFORM HMEIMS-READ-SEC
            IF  HMEIMS-INV-FLG = "INV"
                MOVE  "R"    TO  EDIT-OPTION OF DSP-NFSYOC
                IF ERR-MSG-CD = ZERO
                   MOVE  13  TO  ERR-MSG-CD
                   MOVE  "C" TO  EDIT-CURSOR OF DSP-NFSYOC
                END-IF
            ELSE
                MOVE MEI-F021 TO DSP-SHON1
                MOVE MEI-F022 TO DSP-SHON2
                MOVE MEI-F031 TO DSP-KANA1
                MOVE MEI-F032 TO DSP-KANA2
            END-IF
        END-IF
     ELSE               *>修正／削除の時
        MOVE  SPACE              TO  DSP-MEISAI *>初期化
        MOVE  SHO-F02            TO  DSP-SHOCD  *>サカタ商品ＣＤ
        MOVE  SHO-F03(1:5)       TO  DSP-TAN1   *>品単１
        MOVE  SHO-F03(6:2)       TO  DSP-TAN2   *>品単２
        MOVE  SHO-F03(8:1)       TO  DSP-TAN3   *>品単３
        MOVE  SHO-F04            TO  DSP-JANCD  *>ＪＡＮＣＤ
        MOVE  SHO-F05            TO  DSP-SHON1  *>商品名漢字
        MOVE  SHO-F06            TO  DSP-SHON2  *>規格名漢字
        MOVE  SHO-F07            TO  DSP-KANA1  *>商品名カナ
        MOVE  SHO-F08            TO  DSP-KANA2  *>規格名カナ
        MOVE  SHO-F09            TO  DSP-IRESU  *>入数
        MOVE  SHO-F11            TO  DSP-GENTAN *>標準原価単価
        MOVE  SHO-F12            TO  DSP-URITAN *>標準売価単価
        MOVE  SHO-F13            TO  DSP-GENKA1 *>ランク原単価１
        MOVE  SHO-F14            TO  DSP-BAIKA1 *>ランク売単価１
        MOVE  SHO-F15            TO  DSP-GENKA2 *>ランク原単価２
        MOVE  SHO-F16            TO  DSP-BAIKA2 *>ランク売単価２
        MOVE  SHO-F17            TO  DSP-GENKA3 *>ランク原単価３
        MOVE  SHO-F18            TO  DSP-BAIKA3 *>ランク売単価３
        MOVE  SHO-F19            TO  DSP-GENKA4 *>ランク原単価４
        MOVE  SHO-F20            TO  DSP-BAIKA4 *>ランク売単価４
        MOVE  SHO-F21            TO  DSP-GENKA5 *>ランク原単価５
        MOVE  SHO-F22            TO  DSP-BAIKA5 *>ランク売単価５
        MOVE  SHO-F23            TO  DSP-BUMON  *>部門
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
     MOVE  "BODY"           TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**  ボディー部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "2"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
       WHEN  "F009"
         MOVE   "2"         TO  MAIN-FLG
         MOVE   ZERO        TO  ERR-MSG-CD
       WHEN  "E000"
         PERFORM  BODYCHK-SUB
         IF ERR-MSG-CD = ZERO
            PERFORM  BODYDEL-SUB
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
*
*****MOVE  SPACE            TO  DSP-MEISAI.
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SHOCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHOCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-JANCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JANCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SHON1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHON2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KANA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANA1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KANA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANA2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-IRESU.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-IRESU.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENTAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENTAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-URITAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-URITAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA1
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA2
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA3
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA4.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA4
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA5.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA5.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BUMON.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BUMON.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ入力チェック                                      *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*
     MOVE  ZERO             TO  ERR-MSG-CD.
*サカタ商品ＣＤ入力チェック
     IF DSP-SHOCD = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SHOCD
        IF ERR-MSG-CD = ZERO
           MOVE  03         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SHOCD
        END-IF
     ELSE
********商品マスタ存在チェック
********商品変換テーブル情報をセット
            MOVE DSP-SHOCD   TO  MEI-F011
            MOVE DSP-TAN1    TO  MEI-F0121
            MOVE DSP-TAN2    TO  MEI-F0122
            MOVE DSP-TAN3    TO  MEI-F0123
************商品名取得の為、商品名称マスタ索引
            PERFORM HMEIMS-READ-SEC
            IF  HMEIMS-INV-FLG = "INV"
                MOVE  "R"    TO  EDIT-OPTION OF DSP-SHOCD
                MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN1
                MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN2
                MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN3
                IF ERR-MSG-CD = ZERO
                   MOVE  04  TO  ERR-MSG-CD
                   MOVE  "C" TO  EDIT-CURSOR OF DSP-SHOCD
                END-IF
            END-IF
     END-IF.
*ＪＡＮＣＤ未入力チェック
     IF DSP-JANCD = SPACE
     OR DSP-JANCD(1:1) = SPACE
     OR DSP-JANCD(2:1) = SPACE
     OR DSP-JANCD(3:1) = SPACE
     OR DSP-JANCD(4:1) = SPACE
     OR DSP-JANCD(5:1) = SPACE
     OR DSP-JANCD(6:1) = SPACE
     OR DSP-JANCD(7:1) = SPACE
     OR DSP-JANCD(8:1) = SPACE
     OR DSP-JANCD(9:1) = SPACE
     OR DSP-JANCD(10:1) = SPACE
     OR DSP-JANCD(11:1) = SPACE
     OR DSP-JANCD(12:1) = SPACE
     OR DSP-JANCD(13:1) = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-JANCD
        IF ERR-MSG-CD = ZERO
           MOVE  05         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-JANCD
        END-IF
     END-IF.
*商品名漢字入力チェック
     IF DSP-SHON1 = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SHON1
        IF ERR-MSG-CD = ZERO
           MOVE  14         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SHON1
        END-IF
     END-IF.
*商品名カナ入力チェック
     IF DSP-KANA1 = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KANA1
        IF ERR-MSG-CD = ZERO
           MOVE  15         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KANA1
        END-IF
     END-IF.
*入数は特にチェックなし
*標準原価単価
     IF DSP-GENTAN NOT NUMERIC
     OR DSP-GENTAN  = ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-GENTAN
        IF ERR-MSG-CD = ZERO
           MOVE  06         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENTAN
        END-IF
     END-IF.
*標準売価単価
     IF DSP-URITAN NOT NUMERIC
     OR DSP-URITAN  =  ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-URITAN
        IF ERR-MSG-CD = ZERO
           MOVE  07         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-URITAN
        END-IF
     END-IF.
*原価／売価大小チェック
     IF  DSP-GENTAN  NUMERIC
     AND DSP-URITAN  NUMERIC
        IF  DSP-GENTAN > DSP-URITAN
            MOVE  "R"           TO  EDIT-OPTION OF DSP-GENTAN
            MOVE  "R"           TO  EDIT-OPTION OF DSP-URITAN
            IF ERR-MSG-CD = ZERO
               MOVE  08         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENTAN
            END-IF
        END-IF
     END-IF.
*ランク原価／売価大小チェック１
     IF  DSP-GENKA1 > DSP-BAIKA1
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA1
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA1
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA1
         END-IF
     END-IF.
*ランク原価／売価大小チェック２
     IF  DSP-GENKA2 > DSP-BAIKA2
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA2
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA2
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA2
         END-IF
     END-IF.
*ランク原価／売価大小チェック３
     IF  DSP-GENKA3 > DSP-BAIKA3
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA3
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA3
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA3
         END-IF
     END-IF.
*ランク原価／売価大小チェック４
     IF  DSP-GENKA4 > DSP-BAIKA4
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA4
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA4
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA4
         END-IF
     END-IF.
*ランク原価／売価大小チェック５
     IF  DSP-GENKA5 > DSP-BAIKA5
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA5
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA5
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA5
         END-IF
     END-IF.
*部門チェック
     IF DSP-BUMON = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-BUMON
        IF ERR-MSG-CD = ZERO
           MOVE  16         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-BUMON
        END-IF
     END-IF.
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*    確認入力                                                  *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF ERR-MSG-CD = ZERO
        MOVE  "Y"           TO  DSP-KAKUNI
     END-IF.

     PERFORM  MSG-SEC.
     IF WK-SYORI = 3
        MOVE  PMSG01        TO  DSP-MSG2
     ELSE
        MOVE  PMSG02        TO  DSP-MSG2
     END-IF.

     PERFORM  DSP-WRITE-SUB.
     MOVE  "KAKU"           TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
     MOVE  ZERO             TO  ERR-MSG-CD.
 KAKUNIN.
** 確認
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
         MOVE  SPACE        TO  DSP-KAKUNI
       WHEN  "F009"
         IF WK-SYORI = 3
            MOVE  01        TO  ERR-MSG-CD
         ELSE
            MOVE  "3"       TO  MAIN-FLG
         END-IF
         MOVE  SPACE        TO  DSP-KAKUNI
       WHEN  "E000"
         IF DSP-KAKUNI NOT = "Y"
            MOVE  06        TO  ERR-MSG-CD
         ELSE
            MOVE  SPACE     TO  DSP-KAKUNI
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
        MOVE  SPACE         TO  SHO-REC
        INITIALIZE              SHO-REC
     END-IF.
*
     MOVE  DSP-NFSYOC       TO  SHO-F01.
     MOVE  DSP-SHOCD        TO  SHO-F02.
     MOVE  DSP-TAN1         TO  SHO-F03(1:5).
     MOVE  DSP-TAN2         TO  SHO-F03(6:2).
     MOVE  DSP-TAN3         TO  SHO-F03(8:1).
     MOVE  DSP-JANCD        TO  SHO-F04.
     MOVE  DSP-SHON1        TO  SHO-F05.
     MOVE  DSP-SHON2        TO  SHO-F06.
     MOVE  DSP-KANA1        TO  SHO-F07.
     MOVE  DSP-KANA2        TO  SHO-F08.
     MOVE  DSP-IRESU        TO  SHO-F09.
     MOVE  DSP-GENTAN       TO  SHO-F11.
     MOVE  DSP-URITAN       TO  SHO-F12.
     MOVE  DSP-GENKA1       TO  SHO-F13.
     MOVE  DSP-BAIKA1       TO  SHO-F14.
     MOVE  DSP-GENKA2       TO  SHO-F15.
     MOVE  DSP-BAIKA2       TO  SHO-F16.
     MOVE  DSP-GENKA3       TO  SHO-F17.
     MOVE  DSP-BAIKA3       TO  SHO-F18.
     MOVE  DSP-GENKA4       TO  SHO-F19.
     MOVE  DSP-BAIKA4       TO  SHO-F20.
     MOVE  DSP-GENKA5       TO  SHO-F21.
     MOVE  DSP-BAIKA5       TO  SHO-F22.
*部門
     MOVE  DSP-BUMON        TO  SHO-F23.
*担当者情報更新
     MOVE  PARA-BUMONCDI    TO  SHO-F97.
     MOVE  PARA-TANCDI(1:2) TO  SHO-F98.
     MOVE  SYS-DATE         TO  SHO-F99.
*    処理モードにより追加・更新・削除
     EVALUATE  WK-SYORI
       WHEN  1
         MOVE  PARA-BUMONCDI    TO  SHO-F94
         MOVE  PARA-TANCDI(1:2) TO  SHO-F95
         MOVE  SYS-DATE         TO  SHO-F96
         WRITE  SHO-REC
       WHEN  2
         REWRITE  SHO-REC
       WHEN  3
         DELETE  NFSHOMS
     END-EVALUATE.
*
     PERFORM  HEADDEL-SUB.
     MOVE  SPACE            TO  DSP-MEISAI.
     PERFORM  BODYDEL-SUB.
     MOVE  "2"              TO  MAIN-FLG.

 FILPRT-END.
     EXIT.
*--------------------------------------------------------------*
*    商品変換テーブル読込                                      *
*--------------------------------------------------------------*
 HSHOTBL-READ-SEC       SECTION.
*
     READ  HSHOTBL
           INVALID      MOVE "INV" TO HSHOTBL-INV-FLG
           NOT INVALID  MOVE SPACE TO HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称マスタ読込                                       *
*--------------------------------------------------------------*
 HMEIMS-READ-SEC        SECTION.
*
     READ  HMEIMS
           INVALID      MOVE "INV" TO HMEIMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    担当者マスタ読込                                          *
*--------------------------------------------------------------*
 HTANMS-READ-SEC        SECTION.
*
     READ  HTANMS
           INVALID      MOVE "INV" TO HTANMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
     CLOSE  DSPF.
     CLOSE  NFSHOMS.
     CLOSE  HSHOTBL  HMEIMS  HTANMS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
