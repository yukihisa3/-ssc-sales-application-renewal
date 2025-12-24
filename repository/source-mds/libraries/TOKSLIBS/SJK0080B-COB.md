# SJK0080B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK0080B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム　　　　　　　　　*
*    業務名　　　　　　　：　欠品リスト　　　　　　　　        *
*    モジュール名　　　　：　オンラインデータ欠品ＣＳＶ出力　　*
*    作成日／更新日　　　：　15/10/14                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　                      *
*    処理概要　　　　　　：　欠品データをＣＳＶ用ファイルへ出力*
*                            する。　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJK0080B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         15/10/14.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*欠品ファイル
     SELECT  KPNDATF   ASSIGN    TO        KPNDATL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KPN-F46
                                           KPN-F47
                                           KPN-F01
                                           KPN-F48
                                           KPN-F02
                                           KPN-F04
                                           KPN-F132
                                           KPN-F03
                       FILE      STATUS    KPN-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*
*欠品出力用ファイル
     SELECT  CSVKEPF   ASSIGN    TO        CSVKEPL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEP-F01
                                           KEP-F05
                                           KEP-F09
                                           KEP-F03
                                           KEP-F04
                       FILE      STATUS    KEP-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 欠品ファイル                     *
****************************************************************
 FD  KPNDATF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      KPNDATF   OF   XFDLIB
                       JOINING   KPN       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE= 店舗マスタ                                          *
****************************************************************
 FD  HTENMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 欠品出力用ファイル                                 *
****************************************************************
 FD  CSVKEPF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      CSVKEPF   OF   XFDLIB
                       JOINING   KEP       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KPN-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  TEN-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  KEP-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
 01  READ-CNT                     PIC  9(08)  VALUE  0.
 01  HTOKMS-INV-FLG               PIC  X(03)  VALUE  SPACE.
 01  HTENMS-INV-FLG               PIC  X(03)  VALUE  SPACE.
 01  ZSOKMS-INV-FLG               PIC  X(03)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
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
*
 01  FILE-ERR.
     03  KPN-ERR           PIC N(20) VALUE
                        NC"欠品ファイルエラー".
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TEN-ERR           PIC N(20) VALUE
                        NC"店舗マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  KEP-ERR           PIC N(20) VALUE
                        NC"欠品出力用ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KPN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE KPNDATF.
     DISPLAY     KPN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KPN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KEP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CSVKEPF.
     DISPLAY     KEP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KEP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*                   MAIN       MODULE                3.0       *
****************************************************************
 PROCESS-SEC             SECTION.
     MOVE     "PROCESS-SEC" TO   S-NAME.
     DISPLAY  "***   SJK0080B   START    ***"  UPON CONS.
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC   UNTIL     END-FLG  =   "END".
     PERFORM   END-SEC.
     STOP      RUN.
*
 PROCESS-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイル初期処理                 4.0       *
****************************************************************
 INIT-SEC                SECTION.
     MOVE     "INIT-SEC"    TO   S-NAME.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
*ファイルのＯＰＥＮ
     OPEN     INPUT     KPNDATF HTOKMS   HTENMS   ZSOKMS.
     OPEN     I-O       CSVKEPF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*
     PERFORM  KPN-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             ＰＲＴファイルメイン処理               5.0       *
****************************************************************
 MAIN-SEC                SECTION.
     MOVE     "MAIN-SEC"    TO   S-NAME.
*欠品出力用ファイルを出力する。
     MOVE     SPACE         TO   KEP-REC
     INITIALIZE                  KEP-REC.
*取引先ＣＤ
     MOVE     KPN-F01       TO   KEP-F01.
*取引先名
     MOVE     KPN-F01       TO   TOK-F01  TEN-F52.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = "INV"
         MOVE ALL NC"＊"    TO   KEP-F02
     ELSE
         MOVE TOK-F02       TO   KEP-F02
     END-IF.
*伝票番号
     MOVE     KPN-F02       TO   KEP-F03.
*行番号
     MOVE     KPN-F03       TO   KEP-F04.
*店舗ＣＤ／店舗名
     MOVE     KPN-F07       TO   KEP-F05  TEN-F011.
     PERFORM  HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG = "INV"
         MOVE ALL NC"＊"    TO   KEP-F06
     ELSE
         MOVE TEN-F02       TO   KEP-F06
     END-IF.
*出荷場所／出荷場所名
     MOVE     KPN-F08       TO   KEP-F07  SOK-F01.
     PERFORM  ZSOKMS-READ-SEC.
     IF  ZSOKMS-INV-FLG = "INV"
         MOVE ALL NC"＊"    TO   KEP-F08
     ELSE
         MOVE SOK-F02       TO   KEP-F08
     END-IF.
*納品日
     MOVE     KPN-F112      TO   KEP-F09.
*相手商品ＣＤ
     MOVE     KPN-F25       TO   KEP-F10.
*サカタ商品／品単／商品名
     MOVE     KPN-F1411     TO   KEP-F11.
     MOVE     KPN-F1412(1:5) TO  KEP-F12.
     MOVE     KPN-F1412(6:2) TO  KEP-F13.
     MOVE     KPN-F1412(8:1) TO  KEP-F14.
     MOVE     KPN-F1421      TO  KEP-F15.
     MOVE     KPN-F1422      TO  KEP-F16.
*受注数量／出荷数量／欠品数量
     MOVE     KPN-F50        TO  KEP-F17.
     MOVE     KPN-F15        TO  KEP-F18.
     COMPUTE  KEP-F19  =  KEP-F17  -  KEP-F18.
*自動欠品ＦＬＧセット
     MOVE     KPN-F70        TO  KEP-F20.
*レコード出力
     WRITE    KEP-REC.
*
     PERFORM  KPN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             　欠品ファイルＲＥＡＤ　                       *
****************************************************************
 KPN-READ-SEC          SECTION.
     MOVE     "KPN-READ-SEC"      TO   S-NAME.
*
     READ     KPNDATF       AT    END
              MOVE         "END"  TO   END-FLG
              GO   TO   KPN-READ-EXIT
         NOT  AT   END
              ADD        1  TO    READ-CNT
     END-READ.
*
     IF       READ-CNT(6:3)  =  "000"  OR "500"
              DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 KPN-READ-EXIT.
     EXIT.
****************************************************************
*   取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC       SECTION.
*
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ  HTOKMS
           INVALID     MOVE   "INV"   TO   HTOKMS-INV-FLG
           NOT INVALID MOVE   SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*   店舗マスタ読込
****************************************************************
 HTENMS-READ-SEC       SECTION.
*
     MOVE     "HTENMS-READ-SEC"   TO   S-NAME.
*
     READ  HTENMS
           INVALID     MOVE   "INV"   TO   HTENMS-INV-FLG
           NOT INVALID MOVE   SPACE   TO   HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*   倉庫マスタ読込
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
*
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.
*
     READ  ZSOKMS
           INVALID     MOVE   "INV"   TO   ZSOKMS-INV-FLG
           NOT INVALID MOVE   SPACE   TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               6.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             KPNDATF  HTOKMS  HTENMS  CSVKEPF
                       ZSOKMS.
     DISPLAY  "***   KPN-CNT = " READ-CNT " *** " UPON CONS.
     DISPLAY  "***   SJK0080B   END      ***"  UPON CONS.
**
 END-EXIT.
     EXIT.
*****************<<  SSY0801L   END PROGRAM  >>******************

```
