# SSY3810I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3810I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム            *
*    業務名　　　　　　　：　ナフコ出荷　　　　　　　　　      *
*    モジュール名　　　　：　ナフコ店舗マスタ保守              *
*    処理概要　　　　　　：　ナフコ店舗マスタの登録、修正、削　*
*                            除を行なう。　                    *
*    作成日／作成者　　　：　15/05/07 NAV TAKAHASHI            *
*    更新日／更新者　　　：　2019/12/03 INOUE                  *
*                        ：　  S2245053 項目追加
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3810I.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
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
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYK-F01
                                                 JYK-F02
                        FILE      STATUS    IS   JYK-STATUS.
*---<<　ナフコ店舗マスタ  >>---*
     SELECT   NFTENMS   ASSIGN    TO        DA-01-VI-NFTENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   NFT-F01
                                                 NFT-F02
                           FILE      STATUS  IS  NFT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FSY38101  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  担当者マスタ  >>---*
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  条件マスタ  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYK       PREFIX.
*---<<　ナフコ店舗マスタ  >>---*
 FD  NFTENMS.
     COPY     NFTENMS   OF        XFDLIB
              JOINING   NFT       PREFIX.
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
     02  TOK-STATUS          PIC  X(02).
     02  JYK-STATUS          PIC  X(02).
     02  NFT-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  HTANMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  NFTENMS-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HJYOKEN-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
     02  WK-ADD              PIC  9(07)  VALUE ZERO.
     02  WK-UPDATE           PIC  9(07)  VALUE ZERO.
     02  WK-DELETE           PIC  9(07)  VALUE ZERO.
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
*部門＋担当者ＣＤ
 01  HEN-BUMONTANCD.
     03  HEN-BUMON                PIC  X(04)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  HEN-TANCD                PIC  X(02)  VALUE  SPACE.
*
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_項目戻り".
     02  PMSG03            PIC N(20) VALUE
                           NC"_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY3810I".
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
            NC"取引先コードを入力して下さい".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"取引先マスタが未登録です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"店舗コードを入力して下さい".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"店舗名正式が未入力です".
     02  MSG-ERR8            PIC  N(28)  VALUE
            NC"店舗名略称が未入力です".
     02  MSG-ERR9            PIC  N(28)  VALUE
            NC"店舗名カナが未入力です。".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"郵便番号が未入力です".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"住所１が未入力です。".
     02  MSG-ERR12           PIC  N(28)  VALUE
            NC"県コードが未入力です。".
     02  MSG-ERR13           PIC  N(28)  VALUE
            NC"県コード未登録です（条件Ｆ）".
     02  MSG-ERR14           PIC  N(28)  VALUE
            NC"デリバリ区分に誤りがあります".
     02  MSG-ERR15           PIC  N(28)  VALUE
            NC"Ｃ／Ｄに誤りがあります".
     02  MSG-ERR16           PIC  N(28)  VALUE
            NC"所属エリアが未登録です".
     02  MSG-ERR17           PIC  N(28)  VALUE
            NC"ナフコ店舗マスタ未登録です".
     02  MSG-ERR18           PIC  N(28)  VALUE
            NC"ナフコ店舗マスタに登録済です".
     02  MSG-ERR19           PIC  N(28)  VALUE
            NC"処理区分に誤りがあります".
     02  MSG-ERR20           PIC  N(28)  VALUE
            NC"納品場所ＣＤ入力時は必須です".
     02  MSG-ERR21           PIC  N(28)  VALUE
            NC"ナフコ法人コードは必須です".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  21   TIMES.
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
                     PROCEDURE    HTOKMS.
     MOVE     "TOKMS2"       TO   ERR-FL-ID.
     MOVE     TOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    NFTENMS.
     MOVE     "NFTENMS1"     TO   ERR-FL-ID.
     MOVE     NFT-STATUS     TO   ERR-STCD.
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
     OPEN  I-O    NFTENMS.
     OPEN  INPUT  HJYOKEN  HTANMS  HTOKMS.

     MOVE  "FSY38101"        TO  DSP-FORMAT.
     MOVE  SPACE            TO  DSP-FSY38101.
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
     MOVE  PMSG03           TO  DSP-FNCSPC.
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
            PERFORM  BODYDEL-SUB
            MOVE     SPACE     TO  DSP-MAS003
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
*    システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM  DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"     USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DSP-SDATE.
*    システム日付取得
     ACCEPT  WK-TIME  FROM TIME.
*    画面表示時刻編集
     MOVE  WK-TIME(1:6)     TO  DSP-STIME.
     MOVE  "SSY3810I"       TO  DSP-PGID.
     MOVE  "FSY38101"       TO  DSP-FORMID.
     WRITE  DSP-FSY38101.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*    エラーメッセージセット                                    *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
*    エラー メッセージ セット
     IF ERR-MSG-CD = ZERO
        MOVE  SPACE         TO  DSP-MSGSPC
     ELSE
        MOVE  ERR-MSG(ERR-MSG-CD)  TO  DSP-MSGSPC
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
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-MODE.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-MODE.
*    処理区分 CHK
     IF ( DSP-MODE  NOT  NUMERIC   )
        MOVE  19            TO  ERR-MSG-CD
        MOVE  "R"           TO  EDIT-OPTION OF DSP-MODE
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-MODE
     ELSE
        IF ( DSP-MODE = 1 OR 2 OR 3 )
           IF DSP-MODE = 1
              MOVE  1       TO  WK-SYORI
           END-IF
           IF DSP-MODE = 2
              MOVE  2       TO  WK-SYORI
           END-IF
           IF DSP-MODE = 3
              MOVE  3       TO  WK-SYORI
           END-IF
        ELSE
           MOVE  19         TO  ERR-MSG-CD
           MOVE  "R"        TO  EDIT-OPTION  OF  DSP-MODE
           MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-MODE
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
     MOVE  PMSG01           TO  DSP-FNCSPC.
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
         MOVE     SPACE     TO  DSP-MAS003
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
*    ＨＥＡＤ部消去                                            *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
     MOVE  SPACE            TO  DSP-MAS001.

     MOVE  "D"              TO  EDIT-OPTION OF DSP-TOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TOKCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENCD.
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部の入力チェック                                  *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*属性初期化
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TOKCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TOKCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENCD.
*取引先ＣＤチェック
     IF DSP-TOKCD  NOT  NUMERIC
     OR DSP-TOKCD  =    ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TOKCD
        IF ERR-MSG-CD = ZERO
           MOVE  03         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TOKCD
        END-IF
     ELSE
********取引先マスタ索引
        MOVE   DSP-TOKCD    TO  TOK-F01
        PERFORM  HTOKMS-READ-SEC
        IF   HTOKMS-INV-FLG = "INV"
             MOVE  "R"           TO  EDIT-OPTION OF DSP-TOKCD
             IF ERR-MSG-CD = ZERO
                MOVE  04         TO  ERR-MSG-CD
                MOVE  "C"        TO  EDIT-CURSOR OF DSP-TOKCD
             END-IF
        ELSE
           MOVE  TOK-F02    TO  DSP-TOKNM
        END-IF
     END-IF.
*店舗ＣＤチェック
     IF DSP-TENCD  NOT  NUMERIC
     OR DSP-TENCD  =    ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TENCD
        IF ERR-MSG-CD = ZERO
           MOVE  05         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TENCD
        END-IF
     END-IF.
*ナフコ店舗マスタ存在チェック
**キーセット
     MOVE DSP-TOKCD         TO  NFT-F01.
     MOVE DSP-TENCD         TO  NFT-F02.
     PERFORM  NFTENMS-READ-SEC.
*****判定
     IF NFTENMS-INV-FLG = "INV"
        IF WK-SYORI NOT = 1
           MOVE  "R"        TO  EDIT-OPTION OF DSP-TOKCD
           MOVE  "R"        TO  EDIT-OPTION OF DSP-TENCD
           IF ERR-MSG-CD = ZERO
              MOVE  17      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-TOKCD
           END-IF
        END-IF
     ELSE
        IF WK-SYORI = 1
           MOVE  "R"        TO  EDIT-OPTION OF DSP-TOKCD
           MOVE  "R"        TO  EDIT-OPTION OF DSP-TENCD
           IF ERR-MSG-CD = ZERO
              MOVE  18      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-TOKCD
           END-IF
        END-IF
     END-IF.
*エラーＣＤチェック
     IF ERR-MSG-CD NOT = ZERO
        GO TO  HEADCHK-END
     END-IF.
*
     PERFORM  FILE-SUB.
*
 HEADCHK-END.
     EXIT.

*--------------------------------------------------------------*
*    ファイルセット                                            *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
     IF WK-SYORI = 1
        MOVE  SPACE              TO  DSP-MAS002
********登録時、伝票番号情報へ初期値セット
********伝票番号採番値
        MOVE  70                 TO  DSP-DENNO
********伝票番号採番範囲開始
        MOVE  70                 TO  DSP-SDENNO
********伝票番号採番範囲終了
        MOVE  999994             TO  DSP-EDENNO
********伝票番号桁数
        MOVE  6                  TO  DSP-DENKET
********Ｃ／Ｄ
        MOVE  1                  TO  DSP-CHKD
     ELSE
        MOVE  SPACE              TO  DSP-MAS002
********納品場所
        MOVE  NFT-F03            TO  DSP-NOUBS
*↓2019.12.03
*       納品場所名漢字
        MOVE  NFT-F22            TO  DSP-BASYON
*       納品場所名カナ
        MOVE  NFT-F23            TO  DSP-BASYOX
*↑2019.12.03
********店舗名正式
        MOVE  NFT-F04            TO  DSP-TENNM
********店舗名略称
        MOVE  NFT-F05            TO  DSP-TENRY
********店舗名カナ
        MOVE  NFT-F06            TO  DSP-TENKAN
********郵便番号／住所／電話／ＦＡＸ
        MOVE  NFT-F07            TO  DSP-YUBIN
        MOVE  NFT-F08            TO  DSP-JYUSY1
        MOVE  NFT-F09            TO  DSP-JYUSY2
        MOVE  NFT-F10            TO  DSP-TELNO
        MOVE  NFT-F11            TO  DSP-FAXNO
********県コード
        MOVE  NFT-F12            TO  DSP-KENCD
        MOVE  "97"               TO  JYK-F01
        MOVE  NFT-F12            TO  JYK-F02
        PERFORM  HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = SPACE
            MOVE  JYK-F03        TO  DSP-KENNM
        ELSE
            MOVE  ALL NC"？"     TO  DSP-KENNM
        END-IF
********エリアＣＤ
        MOVE  NFT-F13            TO  DSP-SAREA
        MOVE  "96"               TO  JYK-F01
        MOVE  NFT-F13            TO  JYK-F02
        PERFORM  HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = SPACE
            MOVE  JYK-F03        TO  DSP-SAREAN
        ELSE
            MOVE  ALL NC"？"     TO  DSP-SAREAN
        END-IF
********デリバリ区分
        MOVE  NFT-F20            TO  DSP-DLBRCD
*↓2019.12.03
*       ナフコ法人ＣＤ
        MOVE  NFT-F21            TO  DSP-HOUJIN
*↑2019.12.03
********ナフコ店舗ＣＤ
        MOVE  NFT-F14            TO  DSP-HGTENP
********伝票番号採番値
        MOVE  NFT-F15            TO  DSP-DENNO
********伝票番号採番範囲開始
        MOVE  NFT-F16            TO  DSP-SDENNO
********伝票番号採番範囲終了
        MOVE  NFT-F17            TO  DSP-EDENNO
********伝票番号桁数
        MOVE  NFT-F18            TO  DSP-DENKET
********Ｃ／Ｄ
        MOVE  NFT-F19            TO  DSP-CHKD
*↓2019.12.03
*       出荷_通番
        MOVE  NFT-F24            TO  DSP-TUUBAN
*↑2019.12.03
********登録　部門＋担当者
        MOVE  NFT-F92            TO  HEN-BUMON  TAN-F01
        MOVE  NFT-F93            TO  HEN-TANCD  TAN-F02
        MOVE  HEN-BUMONTANCD     TO  DSP-TBUMON
********登録担当者名取得
        PERFORM HTANMS-READ-SEC
        IF  HTANMS-INV-FLG = SPACE
            MOVE  TAN-F03        TO  DSP-TTANNM
        ELSE
            MOVE  ALL NC"？"     TO  DSP-TTANNM
        END-IF
        MOVE  NFT-F94            TO  DSP-TDATE
        MOVE  NFT-F95            TO  DSP-TTIME
********更新　部門＋担当者
        MOVE  NFT-F96            TO  HEN-BUMON  TAN-F01
        MOVE  NFT-F97            TO  HEN-TANCD  TAN-F02
        MOVE  HEN-BUMONTANCD     TO  DSP-UBUMON
********登録担当者名取得
        PERFORM HTANMS-READ-SEC
        IF  HTANMS-INV-FLG = SPACE
            MOVE  TAN-F03        TO  DSP-UTANNM
        ELSE
            MOVE  ALL NC"？"     TO  DSP-UTANNM
        END-IF
        MOVE  NFT-F98            TO  DSP-UDATE
        MOVE  NFT-F99            TO  DSP-UTIME
     END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部入力                                            *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG02           TO  DSP-FNCSPC.
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
         MOVE     SPACE     TO  DSP-MAS003
         MOVE  "2"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
       WHEN  "F006"
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
     MOVE  SPACE            TO  DSP-MAS002.

     MOVE  "D"              TO  EDIT-OPTION OF DSP-NOUBS.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NOUBS.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENNM.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENNM.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENRY.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENRY.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENKAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENKAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-YUBIN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YUBIN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-JYUSY1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUSY1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-JYUSY2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUSY2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TELNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TELNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-FAXNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-FAXNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAREA.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAREA.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-DLBRCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DLBRCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-HGTENP.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HGTENP.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-DENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SDENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SDENNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-EDENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-EDENNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-DENKET.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENKET.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-CHKD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHKD.
*↓2019.12.03
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BASYON.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASYON.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BASYOX.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASYOX.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-HOUJIN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HOUJIN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TUUBAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TUUBAN.
*↑2019.12.03
*
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ入力チェック                                      *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.

     MOVE  "D"              TO  EDIT-OPTION OF DSP-NOUBS.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NOUBS.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENNM.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENNM.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENRY.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENRY.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TENKAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TENKAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-YUBIN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-YUBIN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-JYUSY1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUSY1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-JYUSY2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JYUSY2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TELNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TELNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-FAXNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-FAXNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KENCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KENCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAREA.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAREA.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-DLBRCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DLBRCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-HGTENP.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HGTENP.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-DENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SDENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SDENNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-EDENNO.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-EDENNO.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-DENKET.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-DENKET.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-CHKD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-CHKD.
*↓2019.12.03
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BASYON.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASYON.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BASYOX.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BASYOX.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-HOUJIN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-HOUJIN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TUUBAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TUUBAN.
*↑2019.12.03
*↓2019.12.03
*納品場所漢字
     IF DSP-NOUBS   NOT =  SPACE
        IF   DSP-BASYON =  SPACE
             MOVE  "R"           TO  EDIT-OPTION OF DSP-BASYON
             IF ERR-MSG-CD = ZERO
                MOVE  20         TO  ERR-MSG-CD
                MOVE  "C"        TO  EDIT-CURSOR OF DSP-BASYON
             END-IF
     END-IF.
*納品場所カナ
     IF DSP-NOUBS   NOT =  SPACE
        IF   DSP-BASYOX =  SPACE
             MOVE  "R"           TO  EDIT-OPTION OF DSP-BASYOX
             IF ERR-MSG-CD = ZERO
                MOVE  20         TO  ERR-MSG-CD
                MOVE  "C"        TO  EDIT-CURSOR OF DSP-BASYOX
             END-IF
     END-IF.
*↑2019.12.03
*店舗名正式
     IF DSP-TENNM  =  SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TENNM
        IF ERR-MSG-CD = ZERO
           MOVE  07         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TENNM
        END-IF
     END-IF.
*店舗名略称
     IF DSP-TENRY  =  SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TENRY
        IF ERR-MSG-CD = ZERO
           MOVE  08         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TENRY
        END-IF
     END-IF.
*店舗名カナ
     IF DSP-TENKAN  =  SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-TENKAN
        IF ERR-MSG-CD = ZERO
           MOVE  09         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-TENKAN
        END-IF
     END-IF.
*郵便番号
     IF DSP-YUBIN = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-YUBIN
        IF ERR-MSG-CD = ZERO
           MOVE  10         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-YUBIN
        END-IF
     END-IF.
*住所１
     IF DSP-JYUSY1  =  SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-JYUSY1
        IF ERR-MSG-CD = ZERO
           MOVE  11         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-JYUSY1
        END-IF
     END-IF.
*県ＣＤ
     IF DSP-KENCD  NOT NUMERIC
     OR DSP-KENCD  =   ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KENCD
        IF ERR-MSG-CD = ZERO
           MOVE  12         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KENCD
        END-IF
     ELSE
        MOVE  "97"               TO  JYK-F01
        MOVE  DSP-KENCD          TO  JYK-F02
        PERFORM  HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = SPACE
            MOVE  JYK-F03        TO  DSP-KENNM
        ELSE
            MOVE  ALL NC"？"     TO  DSP-KENNM
            MOVE  "R"            TO  EDIT-OPTION OF DSP-KENCD
            IF ERR-MSG-CD = ZERO
               MOVE  13          TO  ERR-MSG-CD
               MOVE  "C"         TO  EDIT-CURSOR OF DSP-KENCD
            END-IF
        END-IF
     END-IF.
*エリアＣＤ
     MOVE  "96"               TO  JYK-F01.
     MOVE  DSP-SAREA          TO  JYK-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = SPACE
         MOVE  JYK-F03        TO  DSP-SAREAN
     ELSE
         MOVE  ALL NC"？"     TO  DSP-SAREAN
         MOVE  "R"            TO  EDIT-OPTION OF DSP-SAREA
         IF ERR-MSG-CD = ZERO
            MOVE  16          TO  ERR-MSG-CD
            MOVE  "C"         TO  EDIT-CURSOR OF DSP-SAREA
         END-IF
     END-IF.
*デリバリ区分
     IF  DSP-DLBRCD  =  SPACE OR  1
         CONTINUE
     ELSE
         MOVE  "R"            TO  EDIT-OPTION OF DSP-DLBRCD
         IF ERR-MSG-CD = ZERO
            MOVE  14          TO  ERR-MSG-CD
            MOVE  "C"         TO  EDIT-CURSOR OF DSP-DLBRCD
         END-IF
     END-IF.
*↓2019.12.03
*ナフコ法人ＣＤ
     IF  ( DSP-HOUJIN   NOT   NUMERIC ) OR
         ( DSP-HOUJIN   =     ZERO    )
           MOVE  "R"            TO  EDIT-OPTION OF DSP-HOUJIN
           IF ERR-MSG-CD = ZERO
              MOVE  21          TO  ERR-MSG-CD
              MOVE  "C"         TO  EDIT-CURSOR OF DSP-HOUJIN
           END-IF
     END-IF.
*↑2019.12.03
*Ｃ／Ｄ
     IF  DSP-CHKD  =  ZERO  OR  1
         CONTINUE
     ELSE
         MOVE  "R"            TO  EDIT-OPTION OF DSP-CHKD
         IF ERR-MSG-CD = ZERO
            MOVE  15          TO  ERR-MSG-CD
            MOVE  "C"         TO  EDIT-CURSOR OF DSP-CHKD
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
        MOVE  "Y"           TO  DSP-ENDCHK
     END-IF.

     PERFORM  MSG-SEC.
     IF WK-SYORI = 3
        MOVE  PMSG01        TO  DSP-FNCSPC
     ELSE
        MOVE  PMSG02        TO  DSP-FNCSPC
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
         MOVE     SPACE     TO  DSP-MAS003
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
         MOVE  SPACE        TO  DSP-ENDCHK

       WHEN  "F006"
         IF WK-SYORI = 3
            MOVE  01        TO  ERR-MSG-CD
         ELSE
            MOVE  "3"       TO  MAIN-FLG
         END-IF
         MOVE  SPACE        TO  DSP-ENDCHK

       WHEN  "E000"
         IF DSP-ENDCHK NOT = "Y"
            MOVE  06        TO  ERR-MSG-CD
         ELSE
            MOVE  SPACE     TO  DSP-ENDCHK
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
        MOVE  SPACE         TO  NFT-REC
        INITIALIZE              NFT-REC
     END-IF.
*
     MOVE  DSP-TOKCD        TO  NFT-F01.
     MOVE  DSP-TENCD        TO  NFT-F02.
     MOVE  DSP-NOUBS        TO  NFT-F03.
     MOVE  DSP-TENNM        TO  NFT-F04.
     MOVE  DSP-TENRY        TO  NFT-F05.
     MOVE  DSP-TENKAN       TO  NFT-F06.
     MOVE  DSP-YUBIN        TO  NFT-F07.
     MOVE  DSP-JYUSY1       TO  NFT-F08.
     MOVE  DSP-JYUSY2       TO  NFT-F09.
     MOVE  DSP-TELNO        TO  NFT-F10.
     MOVE  DSP-FAXNO        TO  NFT-F11.
     MOVE  DSP-KENCD        TO  NFT-F12.
     MOVE  DSP-SAREA        TO  NFT-F13.
     MOVE  DSP-HGTENP       TO  NFT-F14.
     MOVE  DSP-DENNO        TO  NFT-F15.
     MOVE  DSP-SDENNO       TO  NFT-F16.
     MOVE  DSP-EDENNO       TO  NFT-F17.
     MOVE  DSP-DENKET       TO  NFT-F18.
     MOVE  DSP-CHKD         TO  NFT-F19.
     MOVE  DSP-DLBRCD       TO  NFT-F20.
*↓2019.12.03
     MOVE  DSP-HOUJIN       TO  NFT-F21.
     MOVE  DSP-BASYON       TO  NFT-F22.
     MOVE  DSP-BASYOX       TO  NFT-F23.
     MOVE  DSP-TUUBAN       TO  NFT-F24.
*↑2019.12.03
     IF WK-SYORI = 1
********登録部門、担当者ＣＤセット
        MOVE  PARA-BUMONCDI    TO  NFT-F92
        MOVE  PARA-TANCDI      TO  NFT-F93
********システム日付・時刻の取得
        ACCEPT  WK-DATE  FROM  DATE
        MOVE  "3"              TO  LINK-IN-KBN
        MOVE  WK-DATE          TO  LINK-IN-YMD6
        MOVE  ZERO             TO  LINK-IN-YMD8
        MOVE  ZERO             TO  LINK-OUT-RET
        MOVE  ZERO             TO  LINK-OUT-YMD
        CALL  "SKYDTCKB"     USING LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD
        MOVE  LINK-OUT-YMD     TO  DATE-AREA
        MOVE  DATE-AREA        TO  NFT-F94
*       システム日付取得
        ACCEPT  WK-TIME  FROM TIME
*       画面表示時刻編集
        MOVE  WK-TIME(1:6)     TO  NFT-F95
     END-IF.
*****登録部門、担当者ＣＤセット
     MOVE  PARA-BUMONCDI    TO  NFT-F96.
     MOVE  PARA-TANCDI      TO  NFT-F97.
*****システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM  DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"     USING LINK-IN-KBN
                                LINK-IN-YMD6
                                LINK-IN-YMD8
                                LINK-OUT-RET
                                LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
     MOVE  DATE-AREA        TO  NFT-F98.
*    システム日付取得
     ACCEPT  WK-TIME  FROM TIME.
*    画面表示時刻編集
     MOVE  WK-TIME(1:6)     TO  NFT-F99
*    処理モードにより追加・更新・削除
     EVALUATE  WK-SYORI
       WHEN  1
         WRITE    NFT-REC   END-WRITE
         ADD      1         TO  WK-ADD
       WHEN  2
         REWRITE  NFT-REC   END-REWRITE
         ADD      1         TO  WK-UPDATE
       WHEN  3
         DELETE   NFTENMS   END-DELETE
         ADD      1         TO  WK-DELETE
     END-EVALUATE.
*
     PERFORM  HEADDEL-SUB.
     PERFORM  BODYDEL-SUB.
     MOVE     SPACE         TO  DSP-MAS003.
     MOVE  "1"              TO  MAIN-FLG.

 FILPRT-END.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE  DSPF  NFTENMS  HTANMS  HJYOKEN  HTOKMS.
*
     DISPLAY "##" NC"登録" " = " WK-ADD     UPON  CONS.
     DISPLAY "##" NC"更新" " = " WK-UPDATE  UPON  CONS.
     DISPLAY "##" NC"削除" " = " WK-DELETE  UPON  CONS.
*
 END-END.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 HTANMS-READ-SEC             SECTION.
*
     READ  HTANMS
           INVALID     MOVE  "INV"      TO   HTANMS-INV-FLG
           NOT INVALID MOVE  SPACE      TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-END.
     EXIT.
****************************************************************
*    条件ファイル索引
****************************************************************
 HJYOKEN-READ-SEC            SECTION.
*
     READ  HJYOKEN
           INVALID     MOVE  "INV"      TO   HJYOKEN-INV-FLG
           NOT INVALID MOVE  SPACE      TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-END.
     EXIT.
****************************************************************
*    取引先マスタ索引
****************************************************************
 HTOKMS-READ-SEC             SECTION.
*
     READ  HTOKMS
           INVALID     MOVE  "INV"      TO   HTOKMS-INV-FLG
           NOT INVALID MOVE  SPACE      TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-END.
     EXIT.
****************************************************************
*    ナフコ店舗マスタ索引
****************************************************************
 NFTENMS-READ-SEC            SECTION.
*
     READ  NFTENMS
           INVALID     MOVE  "INV"      TO   NFTENMS-INV-FLG
           NOT INVALID MOVE  SPACE      TO   NFTENMS-INV-FLG
     END-READ.
*
 NFTENMS-READ-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
