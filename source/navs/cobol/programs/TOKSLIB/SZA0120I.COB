****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　商品在庫マスタメンテナンス　　　　*
*    作成日／更新日　　　：　93/04/23                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品在庫マスタの登録・修正・削除　*
*                            を行う　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0120I.
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
*
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*
*---<<  商品在庫マスタ（実績）>>---*
     SELECT   ZAMJISF   ASSIGN    TO        DA-01-VI-ZAMJISL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAJ-F01
                                                 ZAJ-F02
                                                 ZAJ-F03
                        FILE      STATUS    IS   ZAJ-STATUS.
*
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  条件ファイル>>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-AREA                PIC  X(2000).
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  商品在庫マスタ（実績） >>---*
 FD  ZAMJISF.
     COPY     ZAMJISF   OF        XFDLIB
              JOINING   ZAJ       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  条件ファイル >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面ファイル  ****
 COPY   FZA01201         OF        XMDLIB.
 COPY   FZA01202         OF        XMDLIB.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  ZAI-STATUS          PIC  X(02).
     02  ZAJ-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  ZAJINV-FLG          PIC  9(01)  VALUE ZERO.
****  処理スイッチ            ****
 01  WK-AREA.
     03  WK-SYORI            PIC  X(01).
****  インデックス            ****
     03  IXA                 PIC  9(02).
     03  IXB                 PIC  9(02).
****  商品右詰め用エリア      ****
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)   OCCURS  8.
****  品単右詰め用エリア      ****
 01  HINTAN                  PIC  X(08).
 01  WK-MIGI.
     03  WK-CNT1                PIC 9(01).
     03  WK-CNT2                PIC 9(01).
     03  MIG-SW                 PIC 9(01).
     03  WK1-HIN                PIC X(05).
     03  WK1-HIN1               REDEFINES   WK1-HIN.
         05  WK1-HIN2               PIC X(01)  OCCURS 5.
     03  WK2-HIN                PIC X(05).
     03  WK2-HIN1               REDEFINES   WK2-HIN.
         05  WK2-HIN2               PIC X(01)  OCCURS 5.
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_戻り".
     02  PMSG03            PIC N(20) VALUE
                           NC"_取消　_終了".
     02  PMSG04            PIC N(20) VALUE
                           NC"_取消　_前頁　".
     02  PMSG05            PIC N(20) VALUE
                           NC"_取消　_終了　_戻り".
     02  PMSG06            PIC N(20) VALUE
                           NC"_取消　_終了　_戻り　_次頁".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0120I".
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
            NC"商品在庫マスタに未登録です。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"倉庫コードが未入力です。".
     02  MSG-ERR5            PIC  N(28)  VALUE
            NC"商品コードが未入力です。".
     02  MSG-ERR6            PIC  N(28)  VALUE
            NC"品単コードが未入力です。".
     02  MSG-ERR7            PIC  N(28)  VALUE
            NC"_番が未入力です。".
     02  MSG-ERR8            PIC  N(28) VALUE
            NC"処理区分が違います".
     02  MSG-ERR9            PIC  N(28) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"商品名称マスタに未登録です。".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"倉庫マスタに未登録です。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
                             OCCURS  11  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMZAIF.
     MOVE     "ZAMZAIF "       TO   ERR-FL-ID.
     MOVE      ZAI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMJISF.
     MOVE     "ZAMJISF "       TO   ERR-FL-ID.
     MOVE      ZAJ-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS.
     MOVE     "ZSOKMS "        TO   ERR-FL-ID.
     MOVE      SOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS "        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE     "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
 SZA0120I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SZA0120I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
*
     OPEN     I-O       DSPF      ZAMZAIF   ZAMJISF.
     OPEN     INPUT     ZSOKMS    HMEIMS    HJYOKEN.
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
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"    TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03             TO   HEN-TOKHAN
     END-READ.
*
     MOVE     SPACE          TO   FZA01201.
     MOVE     SPACE          TO   FZA01202.
*    INITIALIZE                   FZA01201.
*    INITIALIZE                   FZA01202.
     MOVE    "FZA01201"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   FZA01201.
*    INITIALIZE                   FZA01201.
     MOVE    "1"             TO   MAIN-FLG.
     MOVE    " "             TO   WK-SYORI.
     MOVE     SPACE          TO   DSP-PROC.
     PERFORM  TUKI-SET-SEC.
 INIT-END.
     EXIT.
*----------------------------------------------------------*
*      1.1       月セット                                  *
*----------------------------------------------------------*
 TUKI-SET-SEC           SECTION.
* 月セット
     MOVE     " 6"           TO   TUKI(01).
     MOVE     " 7"           TO   TUKI(02).
     MOVE     " 8"           TO   TUKI(03).
     MOVE     " 9"           TO   TUKI(04).
     MOVE     "10"           TO   TUKI(05).
     MOVE     "11"           TO   TUKI(06).
     MOVE     "12"           TO   TUKI(07).
     MOVE     " 1"           TO   TUKI(08).
     MOVE     " 2"           TO   TUKI(09).
     MOVE     " 3"           TO   TUKI(10).
     MOVE     " 4"           TO   TUKI(11).
     MOVE     " 5"           TO   TUKI(12).
 TUKI-SET-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   SYORI-SUB
         WHEN      "2"  PERFORM   HEAD-SUB
         WHEN      "3"  PERFORM   BODY-SUB1
         WHEN      "4"  PERFORM   BODY-SUB2
         WHEN      "5"  PERFORM   KAKUNIN-SUB1
         WHEN      "6"  PERFORM   KAKUNIN-SUB2
         WHEN      "7"  PERFORM   FILPRT-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       処理区分入力                              *
*----------------------------------------------------------*
 SYORI-SUB            SECTION.
     PERFORM         MSG-SEC1.
     MOVE   "D"     TO   EDIT-OPTION  OF  SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOKCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  SHOCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  HTAN1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HTAN1.
     MOVE   "D"     TO   EDIT-OPTION  OF  HTAN2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HTAN2.
     MOVE   "D"     TO   EDIT-OPTION  OF  HTAN3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HTAN3.
     MOVE   "D"     TO   EDIT-OPTION  OF  TANA1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANA1.
     MOVE   "D"     TO   EDIT-OPTION  OF  TANA2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANA2.
     MOVE   "D"     TO   EDIT-OPTION  OF  TANA3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANA3.
     MOVE   "D"     TO   EDIT-OPTION  OF  KAKU01.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KAKU01.
     MOVE   "D"     TO   EDIT-OPTION  OF  KAKU02.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KAKU02.
     MOVE     PMSG03         TO   MSG12.
     PERFORM         DSP-WRITE-SUB1.
     MOVE    "SYORI" TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   ZERO    TO   ERR-MSG-CD
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
                        GO   TO   SYORI-END
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
                        GO   TO   SYORI-END
         WHEN   "E000"
                        PERFORM   SYORICHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "2"     TO   MAIN-FLG
                            MOVE   "D"     TO
                                   EDIT-OPTION  OF  SYORI
                            MOVE   SPACE   TO
                                   EDIT-CURSOR  OF  SYORI
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 SYORI-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   エラー メッセージセット１                 *
*----------------------------------------------------------*
 MSG-SEC1               SECTION.
* エラー メッセージ セット
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE       TO   MSG11
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)  TO   MSG11
         MOVE    ZERO        TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   エラー メッセージセット２                 *
*----------------------------------------------------------*
 MSG-SEC2               SECTION.
* エラー メッセージ セット
     IF  ERR-MSG-CD     =    ZERO
         MOVE    SPACE       TO   MSG12
     ELSE
         MOVE    ERR-MSG(ERR-MSG-CD)  TO   MSG12
         MOVE    ZERO        TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.2.1   画面表示処理１                            *
*----------------------------------------------------------*
 DSP-WRITE-SUB1         SECTION.
     MOVE    "FZA01201"      TO   DSP-FORMAT.
     MOVE    "ALL1  "        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE1.
     MOVE     HEN-TIME       TO   STIME1.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     WRITE    DSP-AREA       FROM FZA01201.
 DSP-WRITE-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.2.2   画面表示処理２                            *
*----------------------------------------------------------*
 DSP-WRITE-SUB2         SECTION.
     MOVE    "FZA01202"      TO   DSP-FORMAT.
     MOVE    "ALL2  "        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE2.
     MOVE     HEN-TIME       TO   STIME2.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHA2.
     WRITE    DSP-AREA       FROM FZA01202.
 DSP-WRITE-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.3     画面データの入力処理                      *
*----------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
     IF  DSP-FORMAT  = "FZA01201"
         MOVE    DSP-AREA    TO   FZA01201
     ELSE
         MOVE    DSP-AREA    TO   FZA01202
     END-IF.
 DSP-READ-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.4     ＨＥＡＤ部消去                            *
*----------------------------------------------------------*
 HEADDEL-SUB             SECTION.
     MOVE   SPACE  TO   HEAD10.
 HEADDEL-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.5     ＢＯＤＹ部消去                            *
*----------------------------------------------------------*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   BODY10   BODY20.
     PERFORM  TUKI-SET-SEC.
 BODYDEL-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.6     処理区分の入力チェック                    *
*----------------------------------------------------------*
 SYORICHK-SUB             SECTION.
     MOVE       ZERO    TO   ERR-MSG-CD.
*処理区分 CHK
     IF  ( SYORI  NOT  NUMERIC   )
         MOVE   08      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  SYORI
         MOVE   "C"     TO   EDIT-CURSOR  OF  SYORI
     ELSE
         IF  ( SYORI  =  1 OR 2 OR 3 )
             IF  SYORI      =    1
                 MOVE   "1"          TO   WK-SYORI
             END-IF
             IF  SYORI      =    2
                 MOVE   "2"          TO   WK-SYORI
             END-IF
             IF  SYORI      =    3
                 MOVE   "3"          TO   WK-SYORI
             END-IF
         ELSE
             MOVE   08      TO   ERR-MSG-CD
             MOVE   "R"     TO   EDIT-OPTION  OF  SYORI
             MOVE   "C"     TO   EDIT-CURSOR  OF  SYORI
         END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2       ＨＥＡＤ部入力                            *
*----------------------------------------------------------*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC1.
     MOVE   "D"     TO   EDIT-OPTION  OF  KAKU01.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KAKU01.
     MOVE     PMSG05         TO   MSG12.
     PERFORM         DSP-WRITE-SUB1.
     MOVE    "HEAD1" TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        PERFORM                 HEADDEL-SUB
                        PERFORM                 BODYDEL-SUB
                        GO   TO   HEAD-END
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
                        GO   TO   HEAD-END
         WHEN   "F006"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        GO   TO   HEAD-END
         WHEN   "E000"
                        PERFORM   HEADCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            IF   WK-SYORI  =    "3"
                                 MOVE   "5"     TO   MAIN-FLG
                            ELSE
                                 MOVE   "3"     TO   MAIN-FLG
                            END-IF
                            PERFORM  CUR-SET2
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1     ＨＥＡＤ部の入力チェック                  *
*----------------------------------------------------------*
 HEADCHK-SUB             SECTION.
     MOVE       ZERO    TO   ERR-MSG-CD.
****  倉庫コードのチェック  ****
     IF  SOKCD  =  SPACE
         MOVE   04      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
         GO     TO      HEADCHK-END
     ELSE
         PERFORM   SOK-READ-SUB
         IF  ERR-MSG-CD     NOT  =    ZERO
             GO     TO      HEADCHK-END
         END-IF
     END-IF.
****  商品コードのチェック  ****
     IF  ( SHOCD  NOT =  SPACE )
              MOVE    SHOCD        TO  WK-SHOCD
              PERFORM              UNTIL WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IXA FROM 7 BY -1 UNTIL IXA = 0
                         MOVE WK-SHO(IXA)  TO  WK-SHO(IXA + 1)
                 END-PERFORM
                 MOVE    ZERO      TO  WK-SHO(1)
              END-PERFORM
              MOVE    WK-SHOCD     TO  SHOCD
     END-IF.
***** 削除時はノーチェック
     IF  ( SHOCD  =  SPACE ) AND  ( WK-SYORI = "1" OR "2" )
         MOVE   05      TO   ERR-MSG-CD
         MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD
         GO     TO      HEADCHK-END
     END-IF.
****  品単のチェック  ****
     IF  HTAN1  NOT  =  SPACE   AND
         HTAN2  NOT  =  SPACE   AND
         HTAN3  NOT  =  SPACE
****  品単１の右詰め  ****
         PERFORM  HINTAN-SEC
     END-IF.
     MOVE    HTAN1           TO     HINTAN(1:5).
     MOVE    HTAN2           TO     HINTAN(6:2).
     MOVE    HTAN3           TO     HINTAN(8:1).
     PERFORM   SHO-READ-SUB.
     IF  ERR-MSG-CD     NOT  =    ZERO
         GO     TO      HEADCHK-END
     END-IF.
     PERFORM    FILCHK-SUB.
 HEADCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.1   倉庫名の取得                              *
*----------------------------------------------------------*
 SOK-READ-SUB           SECTION.
     MOVE    SOKCD           TO   SOK-F01.
     READ    ZSOKMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF   INVALID-FLG  =  "1"
          MOVE  11       TO   ERR-MSG-CD
          MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
          MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          MOVE  SOK-F02  TO   SOKNM
     END-IF.
 SOK-READ-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.2   商品名の取得                              *
*----------------------------------------------------------*
 SHO-READ-SUB           SECTION.
     MOVE    SHOCD           TO   MEI-F01.
     MOVE    HINTAN          TO   MEI-F012.
     READ    HMEIMS
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF ( INVALID-FLG  =  "1" ) AND ( WK-SYORI = "1" OR "2" )
          MOVE  10       TO   ERR-MSG-CD
          MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD
          MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD
          MOVE   "R"     TO   EDIT-OPTION  OF  HTAN1
          MOVE   "C"     TO   EDIT-CURSOR  OF  HTAN1
          MOVE   "R"     TO   EDIT-OPTION  OF  HTAN2
          MOVE   "C"     TO   EDIT-CURSOR  OF  HTAN2
          MOVE   "R"     TO   EDIT-OPTION  OF  HTAN3
          MOVE   "C"     TO   EDIT-CURSOR  OF  HTAN3
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          MOVE  MEI-F021 TO   SHONM1
          MOVE  MEI-F022 TO   SHONM2
     ELSE
          MOVE  SPACE    TO   SHONM1
          MOVE  SPACE    TO   SHONM2
     END-IF.
 SHO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.3   ファイルの存在チェック                    *
*----------------------------------------------------------*
 FILCHK-SUB             SECTION.
     MOVE    SOKCD           TO   ZAI-F01      ZAJ-F01.
     MOVE    SHOCD           TO   ZAI-F021     ZAJ-F021.
     MOVE    HINTAN          TO   ZAI-F022     ZAJ-F022.
     MOVE    TANA1           TO   ZAI-F03(1:1) ZAJ-F03(1:1).
     MOVE    TANA2           TO   ZAI-F03(2:3) ZAJ-F03(2:3).
     MOVE    TANA3           TO   ZAI-F03(5:2) ZAJ-F03(5:2).
     READ    ZAMZAIF
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
     READ    ZAMJISF
       INVALID      KEY
          MOVE     "1"       TO   ZAJINV-FLG
          MOVE      SPACE    TO   ZAJ-REC
          INITIALIZE              ZAJ-REC
       NOT INVALID  KEY
          MOVE      ZERO     TO   ZAJINV-FLG
     END-READ.
* ＩＮＶＡＬＩＤ処理
     IF   INVALID-FLG  =  "1"
          IF  WK-SYORI     =  "1"
              CONTINUE
          ELSE
              MOVE  03       TO   ERR-MSG-CD
          END-IF
     END-IF.
* ＮＯＴ ＩＮＶＡＬＩＤ処理
     IF  INVALID-FLG   =   ZERO
          IF  WK-SYORI     =   "1"
              MOVE  02       TO   ERR-MSG-CD
          ELSE
              PERFORM    FILE-SUB
          END-IF
     END-IF.
     IF  ERR-MSG-CD  NOT  =  ZERO
         MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
         MOVE   "R"     TO   EDIT-OPTION  OF  SHOCD
         MOVE   "C"     TO   EDIT-CURSOR  OF  SHOCD
         MOVE   "R"     TO   EDIT-OPTION  OF  HTAN1
         MOVE   "C"     TO   EDIT-CURSOR  OF  HTAN1
         MOVE   "R"     TO   EDIT-OPTION  OF  HTAN2
         MOVE   "C"     TO   EDIT-CURSOR  OF  HTAN2
         MOVE   "R"     TO   EDIT-OPTION  OF  HTAN3
         MOVE   "C"     TO   EDIT-CURSOR  OF  HTAN3
         MOVE   "R"     TO   EDIT-OPTION  OF  TANA1
         MOVE   "C"     TO   EDIT-CURSOR  OF  TANA1
         MOVE   "R"     TO   EDIT-OPTION  OF  TANA2
         MOVE   "C"     TO   EDIT-CURSOR  OF  TANA2
         MOVE   "R"     TO   EDIT-OPTION  OF  TANA3
         MOVE   "C"     TO   EDIT-CURSOR  OF  TANA3
     END-IF.
 FILCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.4    ファイルセット                           *
*----------------------------------------------------------*
 FILE-SUB               SECTION.
     MOVE    ZAI-F04           TO   GEN.
     MOVE    ZAI-F05           TO   ZEN.
     MOVE    ZAI-F06           TO   TONSK.
     MOVE    ZAI-F07           TO   TONYK.
     MOVE    ZAI-F08           TO   TOSYK.
     MOVE    ZAI-F09           TO   TOURI.
     MOVE    ZAI-F10           TO   TOHEP.
     MOVE    ZAI-F11           TO   JINYK.
     MOVE    ZAI-F12           TO   JISYK.
     MOVE    ZAI-F13           TO   JIURI.
     MOVE    ZAI-F14           TO   JIHEP.
     MOVE    ZAI-F15           TO   ZENYK.
     MOVE    ZAI-F16           TO   ZESYK.
     MOVE    ZAI-F17           TO   ZEZAI.
     MOVE    ZAI-F18           TO   SYHAC.
     MOVE    ZAI-F19           TO   SYKES.
     MOVE    ZAI-F20           TO   TOHAC.
     MOVE    ZAI-F26           TO   MINYK.
     MOVE    ZAI-F27           TO   MISYK.
     MOVE    ZAI-F28           TO   SUMI.
     MOVE    ZAI-F29           TO   TORCD.
     MOVE    ZAI-F30           TO   KANANM.
     MOVE    ZAI-F98           TO   TOROKU.
     MOVE    ZAI-F99           TO   SYUSEI.
*
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  12
         MOVE    ZAJ-F051(IXA)     TO   HATSU(IXA)
         MOVE    ZAJ-F052(IXA)     TO   NYUKO(IXA)
         MOVE    ZAJ-F053(IXA)     TO   SYUKO(IXA)
     END-PERFORM.
*
     IF   WK-SYORI  = "2"
          MOVE  "3"          TO   MAIN-FLG
     ELSE
          MOVE  "5"          TO   MAIN-FLG
     END-IF.
 FILE-END.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.5    品単右づめ処理                           *
*----------------------------------------------------------*
 HINTAN-SEC            SECTION.
     MOVE     SPACE          TO        WK1-HIN   WK2-HIN.
     MOVE     HTAN1          TO        WK1-HIN.
     MOVE     0              TO        MIG-SW.
     PERFORM     VARYING   IXA       FROM    5  BY  -1
                 UNTIL     IXA <  1   OR  MIG-SW = 1
         IF  ( WK1-HIN2(IXA) NOT  =  SPACE  AND  MIG-SW = 0 )
             MOVE    IXA    TO    WK-CNT1
             MOVE    1      TO    MIG-SW
         END-IF
     END-PERFORM.
     IF   WK-CNT1  =  5
          GO  TO  HINTAN-EXIT
     END-IF.
     COMPUTE   WK-CNT2  =  5 - WK-CNT1.
     PERFORM     VARYING   IXB       FROM    1  BY  1
                 UNTIL     IXB >  WK-CNT1
         ADD       1                TO    WK-CNT2
         MOVE      WK1-HIN2(IXB)    TO    WK2-HIN2(WK-CNT2)
     END-PERFORM.
     MOVE     WK2-HIN        TO        HTAN1.
 HINTAN-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2.1.6    ＨＥＡＤ部　カーソルクリア               *
*----------------------------------------------------------*
 CUR-SET2              SECTION.
     MOVE   "D"     TO   EDIT-OPTION  OF  SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOKCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  SHOCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD.
     MOVE   "D"     TO   EDIT-OPTION  OF  HTAN1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HTAN1.
     MOVE   "D"     TO   EDIT-OPTION  OF  HTAN2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HTAN2.
     MOVE   "D"     TO   EDIT-OPTION  OF  HTAN3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HTAN3.
     MOVE   "D"     TO   EDIT-OPTION  OF  TANA1.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANA1.
     MOVE   "D"     TO   EDIT-OPTION  OF  TANA2.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANA2.
     MOVE   "D"     TO   EDIT-OPTION  OF  TANA3.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANA3.
 CUR-END.
     EXIT.
*----------------------------------------------------------*
*      2.3        ＢＯＤＹ部入力１                         *
*----------------------------------------------------------*
 BODY-SUB1         SECTION.
     PERFORM       MSG-SEC1.
     MOVE     PMSG06         TO   MSG12.
     PERFORM       DSP-WRITE-SUB1.
     MOVE   "BODY1"          TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE      DSP-FUNC
         WHEN   "F004"
                        PERFORM   HEADDEL-SUB
                        PERFORM   BODYDEL-SUB
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        GO   TO   BODY-END
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
                        GO   TO   BODY-END
         WHEN   "F006"
                        MOVE    "2"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        GO   TO   BODY-END
         WHEN   "F012"
                        PERFORM   BODY1CHK-SUB
                        MOVE    "4"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        GO   TO   BODY-END
         WHEN   "E000"
                        PERFORM   BODY1CHK-SUB
                        MOVE    "5"   TO   MAIN-FLG
         WHEN    OTHER
                        MOVE    01     TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.3.1     ＢＯＤＹ１部の数字チェック                *
*----------------------------------------------------------*
 BODY1CHK-SUB      SECTION.
     IF  TORCD   NOT  NUMERIC
         MOVE    ZERO            TO   TORCD
     END-IF.
*
     IF  GEN     NOT  NUMERIC
         MOVE    ZERO            TO   GEN
     END-IF.
     IF  ZEN     NOT  NUMERIC
         MOVE    ZERO            TO   ZEN
     END-IF.
     IF  TONSK   NOT  NUMERIC
         MOVE    ZERO            TO   TONSK
     END-IF.
     IF  TONYK   NOT  NUMERIC
         MOVE    ZERO            TO   TONYK
     END-IF.
     IF  TOSYK   NOT  NUMERIC
         MOVE    ZERO            TO   TOSYK
     END-IF.
     IF  TOURI   NOT  NUMERIC
         MOVE    ZERO            TO   TOURI
     END-IF.
     IF  TOHEP   NOT  NUMERIC
         MOVE    ZERO            TO   TOHEP
     END-IF.
     IF  JINYK   NOT  NUMERIC
         MOVE    ZERO            TO   JINYK
     END-IF.
     IF  JISYK   NOT  NUMERIC
         MOVE    ZERO            TO   JISYK
     END-IF.
     IF  JIURI   NOT  NUMERIC
         MOVE    ZERO            TO   JIURI
     END-IF.
     IF  JIHEP   NOT  NUMERIC
         MOVE    ZERO            TO   JIHEP
     END-IF.
*
     IF  MINYK   NOT  NUMERIC
         MOVE    ZERO            TO   MINYK
     END-IF.
     IF  MISYK   NOT  NUMERIC
         MOVE    ZERO            TO   MISYK
     END-IF.
     IF  SUMI    NOT  NUMERIC
         MOVE    ZERO            TO   SUMI
     END-IF.
     IF  ZENYK   NOT  NUMERIC
         MOVE    ZERO            TO   ZENYK
     END-IF.
     IF  ZESYK   NOT  NUMERIC
         MOVE    ZERO            TO   ZESYK
     END-IF.
     IF  ZEZAI   NOT  NUMERIC
         MOVE    ZERO            TO   ZEZAI
     END-IF.
     IF  TOHAC   NOT  NUMERIC
         MOVE    ZERO            TO   TOHAC
     END-IF.
     IF  SYHAC   NOT  NUMERIC
         MOVE    ZERO            TO   SYHAC
     END-IF.
     IF  SYKES   NOT  NUMERIC
         MOVE    ZERO            TO   SYKES
     END-IF.
 BODY1CHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4        ＢＯＤＹ部入力２                         *
*----------------------------------------------------------*
 BODY-SUB2         SECTION.
     PERFORM       MSG-SEC2.
     MOVE     PMSG04         TO   MSG22.
     PERFORM       DSP-WRITE-SUB2.
     MOVE   "BODY2"          TO   DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE      DSP-FUNC
         WHEN   "F004"
                        PERFORM   HEADDEL-SUB
                        PERFORM   BODYDEL-SUB
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        GO   TO   BODY-END
         WHEN   "F011"
                        MOVE    "3"    TO   MAIN-FLG
                        MOVE   ZERO    TO   ERR-MSG-CD
                        GO   TO   BODY-END
         WHEN   "E000"
                        PERFORM   BODY2CHK-SUB
                        MOVE    "6"   TO   MAIN-FLG
         WHEN    OTHER
                        MOVE    01     TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ＢＯＤＹ２部の数字チェック                *
*----------------------------------------------------------*
 BODY2CHK-SUB      SECTION.
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  12
         IF  HATSU(IXA)  NOT  NUMERIC
             MOVE    ZERO            TO   HATSU(IXA)
         END-IF
         IF  NYUKO(IXA)  NOT  NUMERIC
             MOVE    ZERO            TO   NYUKO(IXA)
         END-IF
         IF  SYUKO(IXA)  NOT  NUMERIC
             MOVE    ZERO            TO   SYUKO(IXA)
         END-IF
     END-PERFORM.
 BODY2CHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力１                                *
*----------------------------------------------------------*
 KAKUNIN-SUB1      SECTION.
     IF  ERR-MSG-CD  =  0
         MOVE     "Y"            TO   KAKU01
     END-IF.
     PERFORM       MSG-SEC1.
     MOVE     PMSG02         TO   MSG12.
     PERFORM       DSP-WRITE-SUB1.
     MOVE    "KAKU01"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        PERFORM    HEADDEL-SUB
                        PERFORM    BODYDEL-SUB
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE  ZERO     TO   ERR-MSG-CD
                        MOVE  SPACE    TO   KAKU01
         WHEN   "F006"
                        IF  WK-SYORI  =  "3"
                            MOVE   "2"      TO   MAIN-FLG
                            MOVE  ZERO      TO   ERR-MSG-CD
                        ELSE
                            MOVE   "3"      TO   MAIN-FLG
                            MOVE  ZERO      TO   ERR-MSG-CD
                        END-IF
                        MOVE  SPACE    TO   KAKU01
         WHEN   "E000"
                        IF  KAKU01  NOT  =  "Y"
                            MOVE    09      TO  ERR-MSG-CD
                        ELSE
                            IF  WK-SYORI  =  "3"
                                MOVE  "6"       TO   MAIN-FLG
                            ELSE
                                MOVE  "4"       TO   MAIN-FLG
                            END-IF
                            MOVE  SPACE    TO   KAKU01
                        END-IF
         WHEN    OTHER
                        MOVE    01     TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.6       確認入力２                                *
*----------------------------------------------------------*
 KAKUNIN-SUB2      SECTION.
     IF  ERR-MSG-CD  =  0
         MOVE     "Y"            TO   KAKU02
     END-IF.
     PERFORM       MSG-SEC2.
     MOVE     PMSG02         TO   MSG22.
     PERFORM       DSP-WRITE-SUB2.
     MOVE    "KAKU02"        TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        PERFORM    HEADDEL-SUB
                        PERFORM    BODYDEL-SUB
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE  ZERO     TO   ERR-MSG-CD
                        MOVE  SPACE    TO   KAKU02
         WHEN   "F006"
                        IF  WK-SYORI  =  "3"
                            MOVE   "5"      TO   MAIN-FLG
                            MOVE  ZERO      TO   ERR-MSG-CD
                        ELSE
                            MOVE   "4"      TO   MAIN-FLG
                            MOVE  ZERO      TO   ERR-MSG-CD
                        END-IF
                        MOVE  SPACE    TO   KAKU02
         WHEN   "E000"
                        IF  KAKU02  NOT  =  "Y"
                            MOVE    09      TO  ERR-MSG-CD
                        ELSE
                            MOVE    "7"    TO   MAIN-FLG
                            MOVE  SPACE    TO   KAKU02
                        END-IF
         WHEN    OTHER
                        MOVE    01     TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.7        ファイル更新                             *
*----------------------------------------------------------*
 FILPRT-SUB             SECTION.
     IF     WK-SYORI    =   "1"
            MOVE    SPACE         TO   ZAI-REC
            INITIALIZE                 ZAI-REC
            MOVE    SYS-DATE      TO   ZAI-F98
            MOVE    SPACE         TO   ZAJ-REC
            INITIALIZE                 ZAJ-REC
     END-IF.
     IF     ZAJINV-FLG  =    1
            MOVE    SPACE         TO   ZAJ-REC
            INITIALIZE                 ZAJ-REC
     END-IF.
     MOVE    SOKCD           TO   ZAI-F01       ZAJ-F01.
     MOVE    SHOCD           TO   ZAI-F021      ZAJ-F021.
     MOVE    HINTAN          TO   ZAI-F022      ZAJ-F022.
     MOVE    TANA1           TO   ZAI-F03(1:1)  ZAJ-F03(1:1).
     MOVE    TANA2           TO   ZAI-F03(2:3)  ZAJ-F03(2:3).
     MOVE    TANA3           TO   ZAI-F03(5:2)  ZAJ-F03(5:2).
     MOVE    GEN             TO   ZAI-F04.
     MOVE    ZEN             TO   ZAI-F05.
     MOVE    TONSK           TO   ZAI-F06.
     MOVE    TONYK           TO   ZAI-F07.
     MOVE    TOSYK           TO   ZAI-F08.
     MOVE    TOURI           TO   ZAI-F09.
     MOVE    TOHEP           TO   ZAI-F10.
     MOVE    JINYK           TO   ZAI-F11.
     MOVE    JISYK           TO   ZAI-F12.
     MOVE    JIURI           TO   ZAI-F13.
     MOVE    JIHEP           TO   ZAI-F14.
     MOVE    ZENYK           TO   ZAI-F15.
     MOVE    ZESYK           TO   ZAI-F16.
     MOVE    ZEZAI           TO   ZAI-F17.
     MOVE    SYHAC           TO   ZAI-F18.
     MOVE    SYKES           TO   ZAI-F19.
     MOVE    TOHAC           TO   ZAI-F20.
     MOVE    MINYK           TO   ZAI-F26.
     MOVE    MISYK           TO   ZAI-F27.
     MOVE    SUMI            TO   ZAI-F28.
     MOVE    TORCD           TO   ZAI-F29.
     MOVE    KANANM          TO   ZAI-F30.
     MOVE    SYS-DATE        TO   ZAI-F99     ZAJ-F99.
*
     PERFORM     VARYING   IXA       FROM    1  BY  1
                 UNTIL     IXA  >  12
         MOVE    HATSU(IXA)     TO   ZAJ-F051(IXA)
         MOVE    NYUKO(IXA)     TO   ZAJ-F052(IXA)
         MOVE    SYUKO(IXA)     TO   ZAJ-F053(IXA)
     END-PERFORM.
*
     EVALUATE    WK-SYORI
         WHEN    "1"   WRITE      ZAI-REC       END-WRITE
                       IF    ZAJINV-FLG     =   1
                             MOVE      SYS-DATE TO   ZAJ-F98
                             WRITE     ZAJ-REC  END-WRITE
                       ELSE
                             REWRITE   ZAJ-REC  END-REWRITE
                       END-IF
         WHEN    "2"   REWRITE    ZAI-REC       END-REWRITE
                       IF    ZAJINV-FLG     =   1
                             MOVE      SYS-DATE TO   ZAJ-F98
                             WRITE     ZAJ-REC  END-WRITE
                       ELSE
                             REWRITE   ZAJ-REC  END-REWRITE
                       END-IF
         WHEN    "3"   DELETE     ZAMZAIF       END-DELETE
                       IF    ZAJINV-FLG     =   0
                             DELETE    ZAMJISF  END-DELETE
                       END-IF
     END-EVALUATE.
     PERFORM    HEADDEL-SUB.
     PERFORM    BODYDEL-SUB.
     MOVE  "1"       TO   MAIN-FLG.
 FILPRT-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      ZAMZAIF   ZAMJISF   ZSOKMS   HMEIMS
              HJYOKEN.
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
