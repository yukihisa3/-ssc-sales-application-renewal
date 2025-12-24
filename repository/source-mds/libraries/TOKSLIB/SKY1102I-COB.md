# SKY1102I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1102I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷管理システム　　　　　　　　　*
*    モジュール名　　　　：　仕入先送り状印刷                  *
*                            （送り状ファイル作成）　          *
*    作成日／作成者　　　：　99/10/05  /HAGIWARA               *
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　仕入先マスタから仕入先送り状　　  *
*                            ファイルの登録を行う　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKY1102I.
 AUTHOR.                HAGIWARA.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITU.
 OBJECT-COMPUTER.       FUJITU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*-----<<  仕入先マスタ  >>-----*
     SELECT   ZSHIMS    ASSIGN    TO        ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHI-F01
                        FILE      STATUS    IS   SHI-STS.
*-----<<  店舗マスタ  >>-----*
*    SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
*                       ORGANIZATION        IS   INDEXED
*                       ACCESS    MODE      IS   DYNAMIC
*                       RECORD    KEY       IS   TEN-F52
*                                                TEN-F011
*                       FILE      STATUS    IS   TEN-STS.
*-----<<  送り状ファイル  >>-----*
     SELECT   SOKURIF   ASSIGN    TO        SOKURIF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OKU-STS.
*-----<<  画面ファイル  >>-----*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
****************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*-----<<  仕入先マスタ  >>-----*
 FD  ZSHIMS.
     COPY     ZSHIMS    OF   XFDLIB  JOINING   SHI  PREFIX.
*-----<<  店舗マスタ  >>-----*
*FD  HTENMS.
*    COPY     HTENMS    OF   XFDLIB  JOINING   TEN  PREFIX.
*-----<<  送り状ファイル  >>-----*
 FD  SOKURIF.
     COPY     SOKURIF   OF   XFDLIB  JOINING   OKU  PREFIX.
*-----<<  画面ファイル  >>-----*
 FD  DSPF.
*01  DSP-AREA                PIC  X(2000).
     COPY     FKY11021  OF   XMDLIB.
****************************************************************
*   ＷＯＲＫＩＮＧ－ＳＴＯＲＡＧＥ　　　ＳＥＣＴＩＯＮ
****************************************************************
 WORKING-STORAGE             SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  SHI-STS             PIC  X(02).
     03  OKU-STS             PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  SYORI-FLG           PIC  9(01)  VALUE ZERO.
     03  INV-FLG             PIC  9(01)  VALUE ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE ZERO.
     03  M-FLG               PIC  9(01)  VALUE ZERO.
     03  K-FLG               PIC  9(01)  VALUE ZERO.
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
****  インデックス            ****
*01  WK-AREA1.
*    03  IX                  PIC  9(03)  VALUE ZERO.
*    03  IY                  PIC  9(03)  VALUE ZERO.
*    03  WK-MAX              PIC  9(03)  VALUE ZERO.
*    03  WK-TOKCD            PIC  9(08)  VALUE ZERO.
****  退避領域                ****
*01  WK-OKU.
*    03  WK-OKUTBL           OCCURS   300.
*        05  WK-TEN          PIC  9(05)  VALUE ZERO.
*        05  WK-YU           PIC  X(06)  VALUE SPACE.
*        05  WK-NM1          PIC  N(15)  VALUE SPACE.
*        05  WK-NM2          PIC  N(15)  VALUE SPACE.
*        05  WK-JYU1         PIC  N(15)  VALUE SPACE.
*        05  WK-JYU2         PIC  N(15)  VALUE SPACE.
*        05  WK-TEL          PIC  X(12)  VALUE SPACE.
*        05  WK-KOSU         PIC  9(02)  VALUE ZERO.
*        05  WK-OIN          PIC  9(04)  VALUE ZERO.
*        05  WK-KIN          PIC  9(04)  VALUE ZERO.
****  ＰＦキーガイド  ***
 01  MSG-AREA.
*    03  PMSG01            PIC N(20) VALUE
*                          NC"_終了".
     03  PMSG02            PIC N(20) VALUE
                           NC"_取消　_終了　_項目戻し".
*    03  PMSG03            PIC N(25) VALUE
*          NC"_取消　_終了　_項目戻し　_前頁　_次頁".
     03  PMSG04            PIC N(20) VALUE
                           NC"_取消　_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKY1102I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージ  ***
 01  ERR-TAB.
     03  MSG1                PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     03  MSG2                PIC  N(28)  VALUE
            NC"１，２，３以外はエラーです。".
     03  MSG3                PIC  N(28)  VALUE
            NC"０，１以外はエラーです。".
     03  MSG4                PIC  N(28)  VALUE
            NC"仕入先コードが未入力です。".
     03  MSG5                PIC  N(28)  VALUE
            NC"仕入先マスタに未登録です。".
     03  MSG6                PIC  N(28)  VALUE
            NC"個数が未入力です。".
     03  MSG7                PIC  N(28)  VALUE
            NC"印刷枚数が未入力です。".
     03  MSG8                PIC  N(28)  VALUE
            NC"Ｙで入力して下さい".
 01  FILLER                  REDEFINES    ERR-TAB.
     03  MSG-TBL             PIC  N(28)  OCCURS   8.
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
                        PROCEDURE   ZSHIMS.
     MOVE     "ZSHIMS "        TO   ERR-FL-ID.
     MOVE      SHI-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
*FILEERR-SEC3           SECTION.
**   USE       AFTER    EXCEPTION
*                       PROCEDURE   HTENMS.
*    MOVE     "HTENMS "        TO   ERR-FL-ID.
*    MOVE      TEN-STS         TO   ERR-STCD.
*    DISPLAY   MSG-ABEND1    UPON   CONS.
*    DISPLAY   MSG-ABEND2    UPON   CONS.
*    MOVE      4000            TO   PROGRAM-STATUS.
*    STOP      RUN.
**
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SOKURIF.
     MOVE     "SOKURIF"        TO   ERR-FL-ID.
     MOVE      OKU-STS         TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
************************************************************
*      _０     メインモジュール                           *
************************************************************
 SKY1102I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SKY1102I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     ZSHIMS.
     OPEN     I-O       DSPF.
     OPEN     OUTPUT    SOKURIF.
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
     MOVE    1               TO   SYORI-FLG.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      SYORI-FLG
*        WHEN      1    PERFORM   INIT1-DSP-SEC
*        WHEN      2    PERFORM   SYORI-SEC
*        WHEN      3    PERFORM   HEAD1-SEC
*        WHEN      4    PERFORM   BODY1-SEC
*        WHEN      5    PERFORM   KAKNIN1-SEC
         WHEN      1    PERFORM   INIT2-DSP-SEC
         WHEN      2    PERFORM   HEAD2-SEC
         WHEN      3    PERFORM   BODY2-SEC
         WHEN      4    PERFORM   KAKNIN2-SEC
     END-EVALUATE.
 MAIN-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    ZSHIMS    SOKURIF   DSPF.
 END-END.
     EXIT.
*----------------------------------------------------------*
*      2.1       初期画面処理   （単一）                   *
*----------------------------------------------------------*
 INIT2-DSP-SEC        SECTION.
     MOVE     SPACE          TO   FKY11021.
     MOVE    "FKY11021"      TO   DSP-FORMAT.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
*
     MOVE     2              TO   SYORI-FLG.
 INIT2-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.2       ＨＥＡＤ２処理 （単一）　　               *
*----------------------------------------------------------*
 HEAD2-SEC            SECTION.
     PERFORM         HEAD-CLR2-SEC.
     MOVE     PMSG04         TO   PFGID2.
     PERFORM         DSP-WRITE-SEC2.
*
     MOVE    "HEAD2" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE   1           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "E000"
                        PERFORM   HEAD2-CHK-SEC
         WHEN    OTHER
                        MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 HEAD2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.1     ＨＥＡＤ部の入力チェック （単一）         *
*----------------------------------------------------------*
 HEAD2-CHK-SEC             SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  R20001
     MOVE   " "     TO   EDIT-CURSOR  OF  R20001
     MOVE  SPACE                 TO  MAS201.
     MOVE  ZERO                  TO  M-FLG.
*仕入先コードチェック
     IF  ( R20001  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   4       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20001
     END-IF.
*仕入先コードＭ存在チェック
     MOVE   R20001        TO    SHI-F01
     PERFORM    SHI-READ-SEC
     IF  INV-FLG     =    1
         IF    ERR-FLG    =  ZERO
               MOVE   5       TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20001
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20001
     ELSE
         PERFORM    SHI-SET-SEC
     END-IF.
*
     IF     ERR-FLG    =     ZERO
            MOVE     3              TO   SYORI-FLG
            PERFORM         HEAD-CLR2-SEC
     END-IF.
 HEAD2-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.7.2.2   仕入先情報ＳＥＴ  （単一）                *
*----------------------------------------------------------*
 SHI-SET-SEC             SECTION.
***  郵便番号
     MOVE     SHI-F121       TO   R20002.
     MOVE     SHI-F122       TO   R20003.
***  送り先１（取引先名）
     MOVE     SHI-F02        TO   R20004.
***  送り先２
     MOVE     SPACE          TO   R20005.
***  住所１
     MOVE     SHI-F07        TO   R20006.
***  住所２
     MOVE     SHI-F08        TO   R20007.
***  電話番号
     MOVE     SHI-F09        TO   R20008.
 SHI-SET-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.8       ＢＯＤＹ２処理 （単一）　　               *
*----------------------------------------------------------*
 BODY2-SEC            SECTION.
     MOVE     PMSG02         TO   PFGID2.
     PERFORM         DSP-WRITE-SEC2.
*
     MOVE    "BODY2" TO   DSP-GROUP.
     PERFORM         DSP-READ-SEC.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                 MOVE   "M"     TO   EDIT-OPTION  OF  R20001
                 MOVE   " "     TO   EDIT-CURSOR  OF  R20001
                        MOVE   1           TO   SYORI-FLG
         WHEN   "F005"
                        MOVE   "END"       TO   END-FLG
         WHEN   "F006"
                 MOVE   "M"     TO   EDIT-OPTION  OF  R20001
                 MOVE   " "     TO   EDIT-CURSOR  OF  R20001
                        PERFORM         HEAD-CLR2-SEC
                        MOVE   2           TO   SYORI-FLG
         WHEN   "E000"
                        PERFORM   BODY2-CHK-SEC
         WHEN    OTHER
                 MOVE   1         TO   ERR-FLG
     END-EVALUATE.
 BODY2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.8.1     ＢＯＤＹ部の入力チェック  （単一）        *
*----------------------------------------------------------*
 BODY2-CHK-SEC             SECTION.
     PERFORM         HEAD-CLR2-SEC.
*個数チェック
     IF  ( R20014  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   6      TO   ERR-FLG

         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20014
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20014
     ELSE
         IF  R20014    =   ZERO
             IF    ERR-FLG    =  ZERO
*******************MOVE   6      TO   ERR-FLG
                   CONTINUE
             END-IF
*************MOVE   "R"     TO   EDIT-OPTION  OF  R20014
*************MOVE   "C"     TO   EDIT-CURSOR  OF  R20014
         END-IF
     END-IF.
*伝票種類チェック
     IF  ( R20010  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   2      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20010
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20010
     ELSE
         IF  (  R20010 =  1  OR  2  OR  3  )
             CONTINUE
         ELSE
             IF    ERR-FLG    =  ZERO
                   MOVE   2      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  R20010
             MOVE   "C"     TO   EDIT-CURSOR  OF  R20010
         END-IF
     END-IF.
*印刷枚数チェック
     IF  ( R20011  NOT  NUMERIC   )
         IF    ERR-FLG    =  ZERO
               MOVE   7      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20011
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20011
     ELSE
         IF  R20011    =   ZERO
             IF    ERR-FLG    =  ZERO
                   MOVE   7      TO   ERR-FLG
             END-IF
             MOVE   "R"     TO   EDIT-OPTION  OF  R20011
             MOVE   "C"     TO   EDIT-CURSOR  OF  R20011
         END-IF
     END-IF.
     IF  ( R20012  NOT  NUMERIC   )
         MOVE   0       TO   K-FLG
     ELSE
         MOVE   1       TO   K-FLG
     END-IF.
*発送元印刷チェック
     IF  ( R20013  NOT  NUMERIC   )
         MOVE   0       TO   R20013
     END-IF.
     IF  ( R20013   =  0 OR 1  )
         CONTINUE
     ELSE
         IF    ERR-FLG    =  ZERO
               MOVE   3      TO   ERR-FLG
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  R20013
         MOVE   "C"     TO   EDIT-CURSOR  OF  R20013
     END-IF.
*
     IF       ERR-FLG   =    ZERO
              MOVE     4              TO   SYORI-FLG
     END-IF.
 BODY2-CHK-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.9       確認入力２    （単一）                    *
*----------------------------------------------------------*
 KAKNIN2-SEC       SECTION.
     MOVE     "Y"            TO   R20015.
     PERFORM       DSP-WRITE-SEC2.
*
     MOVE    "KAKN2"         TO    DSP-GROUP.
     PERFORM       DSP-READ-SEC.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    1      TO   SYORI-FLG
         WHEN   "F005"
                        MOVE  "END"    TO   END-FLG
         WHEN   "F006"
                        MOVE    3      TO   SYORI-FLG
                        MOVE    SPACE  TO   R20015
         WHEN   "E000"
                        IF  R20015  NOT  =  "Y"
                            MOVE    8       TO  ERR-FLG
                        ELSE
                            PERFORM  OKU2-WTR-SEC
                        END-IF
         WHEN    OTHER
                        MOVE    1      TO   ERR-FLG
     END-EVALUATE.
 KAKNIN2-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.9.1      ファイル出力    （単一）                 *
*----------------------------------------------------------*
 OKU2-WTR-SEC           SECTION.
     MOVE   SPACE     TO   OKU-REC.
     INITIALIZE       OKU-REC.
*
***  伝票種類
     MOVE    R20010         TO   OKU-F01.
***  仕入先コード
     MOVE    R20001         TO   OKU-F02.
*****IF    M-FLG    =   1
*****      MOVE    R20002         TO   OKU-F03
*****ELSE
*****      MOVE    ZERO           TO   OKU-F03
*****END-IF.
***  郵便番号
     MOVE    R20002         TO   OKU-F151.
     MOVE    R20003         TO   OKU-F152.
***  名称１（送り先１）
     MOVE    R20004         TO   OKU-F05.
***  名称２（送り先２）
     MOVE    R20005         TO   OKU-F06.
***  住所１
     MOVE    R20006         TO   OKU-F07.
***  住所２
     MOVE    R20007         TO   OKU-F08.
***  電話番号
     MOVE    R20008         TO   OKU-F09.
***  備考
     MOVE    R20009         TO   OKU-F10.
***  個数
     MOVE    R20014         TO   OKU-F11.
***  印刷枚数
     MOVE    R20011         TO   OKU-F12.
***  小札枚数
     IF    K-FLG    =   1
           MOVE    R20012         TO   OKU-F13
     ELSE
           MOVE    ZERO           TO   OKU-F13
     END-IF.
***  発送元印刷
     MOVE    R20013         TO   OKU-F14.
*
     WRITE   OKU-REC.
*
     MOVE    1               TO   SYORI-FLG.
 OKU2-WTR-EXIT.
     EXIT.
*----------------------------------------------------------*
*                仕入先マスタＲＥＡＤ                      *
*----------------------------------------------------------*
 SHI-READ-SEC           SECTION.
     READ    ZSHIMS
       INVALID
          MOVE      1        TO   INV-FLG
       NOT INVALID
          MOVE      0        TO   INV-FLG
     END-READ.
 SHI-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面表示処理２                            *
*----------------------------------------------------------*
 DSP-WRITE-SEC2         SECTION.
     IF    ERR-FLG   =    0
           MOVE    SPACE    TO   ERRMG2
     ELSE
           MOVE    MSG-TBL(ERR-FLG)     TO   ERRMG2
     END-IF.
*
     MOVE    "FKY11021"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
*
     WRITE    FKY11021.
*
     IF    ERR-FLG   NOT   =    0
           MOVE    0        TO   ERR-FLG
     END-IF.
 DSP-WRITE2-EXIT.
     EXIT.
*----------------------------------------------------------*
*                画面データの入力処理                      *
*----------------------------------------------------------*
 DSP-READ-SEC           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
*    IF  DSP-FORMAT  = "ZDA02101"
*        MOVE    DSP-AREA    TO   ZDA02101
*    ELSE
*        MOVE    DSP-AREA    TO   FKY11021
*    END-IF.
 DSP-READ-EXIT.
     EXIT.
*----------------------------------------------------------*
*                ＨＥＡＤ２処理 （単一）　　               *
*----------------------------------------------------------*
 HEAD-CLR2-SEC        SECTION.
     MOVE   " "     TO   EDIT-CURSOR  OF  R20010
                         EDIT-CURSOR  OF  R20011
                         EDIT-CURSOR  OF  R20012
                         EDIT-CURSOR  OF  R20013
                         EDIT-CURSOR  OF  R20014.
     MOVE   "M"     TO   EDIT-OPTION  OF  R20010
                         EDIT-OPTION  OF  R20011
                         EDIT-OPTION  OF  R20012
                         EDIT-OPTION  OF  R20013
                         EDIT-OPTION  OF  R20014.
 HEAD-CLR2-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
