# SNJ0550B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ0550B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバ　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　トリガーファイル作成　　　        *
*    作成日／更新日　　　：　10/08/05                          *
*    作成者／更新者　　　：　ＮＡＶ大野                        *
*    処理概要　　　　　　：　パラメータファイル及び条件ファイル*
*                            より、トリガーファイルを作成する。*
*    作成日／更新日　　　：　12/11/20                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタに受信手順追加／受信取引先*
*                            名の最終桁にＢＭＳのＢを追加する。*
*    更新日／更新者　　　：　2022/10/26 INOUE                  *
*    処理概要　　　　　　：　レイアウト変換区分　セット仕様変更*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0550B.
 AUTHOR.               OONO.
 DATE-WRITTEN.         XX/XX/XX.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信パラメタファイル
     SELECT   PARAFIL   ASSIGN    TO       DA-01-S-PARAFIL
                       FILE   STATUS   IS  PAR-ST.
*トリガーファイル
     SELECT   TRIGGER  ASSIGN    TO        DA-01-S-TRIGGER
                       FILE  STATUS    IS  TRG-ST.
*条件ファイル
     SELECT   JYOKEN1  ASSIGN    TO        DA-01-VI-JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01  JYO-F02
                       FILE  STATUS    IS  JYO-ST.
*
*取引先マスタ
     SELECT   TOKMS2   ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE  STATUS    IS  TOK-ST.
*
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信パラメタファイル                               *
****************************************************************
 FD  PARAFIL           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      PARAFIL   OF   XFDLIB
                       JOINING   PAR       AS   PREFIX.
******************************************************************
*    トリガーファイル
******************************************************************
 FD  TRIGGER           BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      TRIGGER   OF   XFDLIB
                       JOINING   TRG       AS   PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1           LABEL     RECORD    IS   STANDARD.
                       COPY      JYOKEN1   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
*
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2            LABEL     RECORD    IS   STANDARD.
                       COPY      TOKMS2    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PAR-ST                   PIC  X(02).
     03  TRG-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"パラメタファイルエラー".
     03  TRG-ERR           PIC N(15) VALUE
         NC"トリガーファイルエラー".
     03  JYO-ERR           PIC N(15) VALUE
         NC"条件ファイルエラー".
     03  TOK-ERR           PIC N(15) VALUE
         NC"得意先マスタエラー".
*ワーク領域
 01  WK-AREA.
     03  WK-KANRINO               PIC  9(04) VALUE ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-TUSINM           PIC X(01).
 01  LINK-KEKKA            PIC X(01).
*2012/11/20 ↓　追加
 01  LINK-JYUKBN           PIC X(01).
*2012/11/20 ↑　追加
*
**************************************************************
 PROCEDURE             DIVISION      USING    LINK-TUSINM
**********************************************LINK-KEKKA.
*2012/11/20 ↓　追加
                                              LINK-KEKKA
                                              LINK-JYUKBN
*2012/11/20 ↑　追加
**************************************************************
 DECLARATIVES.
 PAR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PARAFIL.
     MOVE        PAR-ST    TO        E-ST.
     MOVE        "PARAFIL" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TRG-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TRIGGER.
     MOVE        TRG-ST    TO        E-ST.
     MOVE        "TRIGGER" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TRG-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYOKEN1.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "JYOKEN1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKMS2.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "TOKMS2"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT   PARAFIL.
     OPEN      INPUT   TOKMS2.
     OPEN      I-O     JYOKEN1.
     OPEN      OUTPUT  TRIGGER.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*    トリガーファイルレコードの初期化
     MOVE      SPACE         TO   TRG-REC.
     INITIALIZE                   TRG-REC.
*
*パラメータファイルの読込
     READ      PARAFIL
               AT END
                  MOVE "9"        TO   LINK-KEKKA
                  GO    TO        MAIN-EXIT
     END-READ.
*条件ファイルのキー項目を設定
     MOVE      24            TO   JYO-F01.
     MOVE      "EDIKANRI"    TO   JYO-F02.
*条件ファイルの読込
     READ      JYOKEN1
               INVALID
                  MOVE "8"        TO   LINK-KEKKA
                  GO    TO        MAIN-EXIT
     END-READ.
*
*管理番号のカウントアップ
     COMPUTE   WK-KANRINO    =    JYO-F04  +  1.
*
     IF   WK-KANRINO   > 9999
          MOVE   JYO-F05     TO   WK-KANRINO
     END-IF.
*
*トリガーファイルの各項目を設定
*    管理番号
     MOVE      WK-KANRINO    TO   TRG-F01.
     MOVE      ","           TO   TRG-C01.
*    端末名
     MOVE      PAR-E06       TO   TRG-F02.
     MOVE      ","           TO   TRG-C02.
*    回線種別　
     IF   PAR-E04   NOT =    SPACE
          MOVE      PAR-E04       TO   TRG-F03
     ELSE
          MOVE      PAR-K01       TO   TRG-F03
     END-IF.
     MOVE      ","           TO   TRG-C03.
*    通信モード分
*****MOVE      PAR-E02       TO   TRG-F04.
     MOVE      LINK-TUSINM   TO   TRG-F04.
     MOVE      ","           TO   TRG-C04.
*    通信手順区分
     MOVE      PAR-E05       TO   TRG-F05.
*2012/11/20　↓　追加
     MOVE      PAR-E05       TO   LINK-JYUKBN.
*2012/11/20　↑　追加
     MOVE      ","           TO   TRG-C05.
*    相手先番号
     MOVE      PAR-E07       TO   TRG-F06.
     MOVE      ","           TO   TRG-C06.
*送信Ｆ番号
     MOVE      PAR-E08       TO   TRG-F07.
     MOVE      ","           TO   TRG-C07.
*送信Ｆ名
     MOVE      PAR-E12       TO   TRG-F08.
     MOVE      ","           TO   TRG-C08.
*レイアウト変換番号
*2010/08/27 3桁化
     IF        PAR-E09 = SPACE
               MOVE      "   "         TO   TRG-F09
     ELSE
          IF   PAR-E09 = 9
               MOVE      999           TO   TRG-F09
          ELSE
               IF   PAR-E09 = 8
                    MOVE      888      TO   TRG-F09
               ELSE
*                   2022/10/26
                    IF   PAR-E09 = 7
                         MOVE      777      TO   TRG-F09
*                   2022/10/26
                    ELSE
                         MOVE  "00"         TO   TRG-F09(1:2)
                         MOVE   PAR-E09     TO   TRG-F09(3:1)
                    END-IF
               END-IF
          END-IF
     END-IF.
*
*取引先マスタ読込　
*2010/11/01
     MOVE      ","           TO   TRG-C09.
     MOVE      X"28"         TO   TRG-F10S.
*
     MOVE      PAR-F03       TO   TOK-F01.
     READ      TOKMS2
               INVALID
                    MOVE   SPACE    TO   TRG-F10
               NOT  INVALID
** 2010/11/15       MOVE   TOK-F03  TO   TRG-F10
                    MOVE   TOK-F03  TO   TRG-F10(1:9)
                    EVALUATE  PAR-E01
                        WHEN  "01"
                              MOVE  NC"発"    TO   TRG-F10(10:1)
                        WHEN  "02"
                              MOVE  NC"受"    TO   TRG-F10(10:1)
                        WHEN  "03"
                              MOVE  NC"支"    TO   TRG-F10(10:1)
                        WHEN  "04"
                              MOVE  NC"請"    TO   TRG-F10(10:1)
                        WHEN  "05"
                              MOVE  NC"出"    TO   TRG-F10(10:1)
                        WHEN  "06"
                              MOVE  NC"欠"    TO   TRG-F10(10:1)
                        WHEN  "07"
                              MOVE  NC"商"    TO   TRG-F10(10:1)
                        WHEN  "08"
                              MOVE  NC"他"    TO   TRG-F10(10:1)
*2012/11/20　↓　追加
                        WHEN  "09"
                              MOVE  NC"Ｂ"    TO   TRG-F10(10:1)
*2012/11/20　↑　追加
                    END-EVALUATE
     END-READ.
*
     MOVE      X"29"         TO   TRG-F10E.
*
* 変換バイト数データセット
     MOVE      ","           TO   TRG-C10.
*
     IF        TRG-F09       =    "999"  OR  "888"
               MOVE     PAR-E10       TO    TRG-F11
     END-IF.
*
*    トリガーファイル登録
     WRITE     TRG-REC.
*    条件ファイル更新
     MOVE      WK-KANRINO    TO   JYO-F04.
     REWRITE   JYO-REC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのＯＰＥＮ
     CLOSE     PARAFIL JYOKEN1 TRIGGER TOKMS2.
*
 END-EXIT.
     EXIT.
*****************<<  SNJ0550B   END PROGRAM  >>******************

```
