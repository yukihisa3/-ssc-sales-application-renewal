# ZMT991B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/ZMT991B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　マスタ管理　　　　　　　　　　　　*
*    業務名　　　　　　　：　マスタメンテ                      *
*    モジュール名　　　　：　商品変換テーブル一括登録          *
*                        ：　ＺＭＴ９９１Ｂ                    *
*    作成日／更新日　　　：　2005/11/08 - 11/09 (1)            *
*    作成者／更新者　　　：　ＮＡＶ武井                        *
*    処理概要　　　　　　：　メンテデータを読み商品変換テーブル*
*                        ：　に登録し，登録済はエラーＦに出力。*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           ZMT991B.
 AUTHOR.               TAKEI.
 DATE-WRITTEN.         05/11/08.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*----<< 一括メンテデータ >>-*
     SELECT  SHTBLM    ASSIGN    TO        DA-01-VI-SHTBLM1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       STM-F031 STM-F032
                                           STM-F01
                       FILE      STATUS    STM-ST.
*----<< 商品変換テーブル >>-*
     SELECT  SHOTBL    ASSIGN    TO        DA-01-VI-SHOTBL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       STB-F01  STB-F02
                       FILE      STATUS    STB-ST.
*----<< エラーファイル >>-*
     SELECT  ERRF      ASSIGN    TO        DA-01-S-ERRF
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    STE-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 一括メンテデータ                                   *
****************************************************************
 FD  SHTBLM
                   BLOCK    CONTAINS  11   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                   COPY      HSHTBLM   OF   XFDLIB
                       JOINING   STM       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル                                   *
****************************************************************
 FD  SHOTBL
                   BLOCK    CONTAINS  12   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                   COPY      HSHOTBL   OF   XFDLIB
                       JOINING   STB       AS   PREFIX.
****************************************************************
*    FILE = エラーファイル                                     *
****************************************************************
 FD  ERRF
                   BLOCK    CONTAINS  11   RECORDS
                       LABEL     RECORD    IS   STANDARD.
                   COPY      HSHTBLM   OF   XFDLIB
                       JOINING   STE       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  STM-ST                   PIC  X(02).
     03  STB-ST                   PIC  X(02).
     03  STE-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  X(03)  VALUE  SPACE.
*カウンター
 01  CNT-AREA.
     03  RD-CNT                   PIC  9(06)  VALUE  ZERO.
     03  SKP-CNT                  PIC  9(06)  VALUE  ZERO.
     03  OUT-CNT                  PIC  9(06)  VALUE  ZERO.
     03  ERR-CNT                  PIC  9(06)  VALUE  ZERO.
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム日付格納（８桁）
 01  WK-DATE.
     03  WK-YY                    PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  STM-ERR           PIC N(15) VALUE
         NC"一括メンテデータエラー".
     03  STB-ERR           PIC N(15) VALUE
         NC"商品変換テーブルエラー".
     03  STE-ERR           PIC N(15) VALUE
         NC"エラーファイルエラー".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-CHK              PIC 9(01).
*
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-CHK.
**************************************************************
 DECLARATIVES.
 STM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTBLM.
     MOVE        STM-ST    TO        E-ST.
     MOVE        "SHTBLM"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STM-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHOTBL.
     MOVE        STB-ST    TO        E-ST.
     MOVE        "SHOTBL"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STB-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ERRF.
     MOVE        STE-ST    TO        E-ST.
     MOVE        "ERRF"    TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STE-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC    UNTIL   END-FLG   =  "END".
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT   SHTBLM.
     OPEN      I-O     SHOTBL.
     OPEN      EXTEND  ERRF.
*システム日付取得／時刻取得
     ACCEPT    SYS-DATE    FROM    DATE.
     ACCEPT    WK-TIME     FROM    TIME.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE      "3"         TO      LINK-IN-KBN.
     MOVE      SYS-DATE    TO      LINK-IN-YMD6.
     MOVE      ZERO        TO      LINK-IN-YMD8.
     MOVE      ZERO        TO      LINK-OUT-RET.
     MOVE      ZERO        TO      LINK-OUT-YMD.
     CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD TO     WK-DATE.
*ＰＧスタートメッセージ
     DISPLAY "***ZMT991B START " WK-YY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*メンテデータ読込み
     PERFORM   SHTBLM-READ-SEC.
*
     IF  END-FLG  =  "END"
               GO  TO  MAIN-EXIT
     END-IF.
*商品変換テーブル読込みチェック
     PERFORM   SHOTBL-READ-SEC.
     IF  INV-FLG  = "INV"
         PERFORM  SHOTBL-WRITE-SEC
     ELSE
         PERFORM  ERRF-WRITE-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     SHTBLM  SHOTBL  ERRF.
*件数メッセージ出力
     DISPLAY   "***ﾒﾝﾃﾃﾞｰﾀ ﾖﾐｺﾐ   ｹﾝｽｳ = "  RD-CNT  UPON CONS.
     DISPLAY   "***ﾄﾘｹｼﾃﾞｰﾀSKIP   ｹﾝｽｳ = "  SKP-CNT UPON CONS.
     DISPLAY   "***ﾍﾝｶﾝﾃｰﾌﾞﾙ ﾄｳﾛｸ ｹﾝｽｳ = "  OUT-CNT UPON CONS.
     DISPLAY   "***ﾄｳﾛｸｴﾗｰ        ｹﾝｽｳ = "  ERR-CNT UPON CONS.
*ＰＧエンドメッセージ
     ACCEPT    WK-TIME  FROM     TIME.
     DISPLAY "***ZMT991B END   " WK-YY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*             一括メンテデータ読込み                 2.1       *
****************************************************************
 SHTBLM-READ-SEC      SECTION.
     MOVE     "SHTBLM-READ-SEC"   TO   S-NAME.
*
 STM010.
     READ      SHTBLM    AT  END
               MOVE     "END"      TO   END-FLG
               GO                  TO   SHTBLM-READ-EXIT
     END-READ.
*
     ADD   1       TO   RD-CNT.
***取消データスキップ
     IF   STM-F21  =  "9"
          ADD     1    TO   SKP-CNT
          GO  TO    STM010
     END-IF.
*
 SHTBLM-READ-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル読込み（乱）           2.2       *
****************************************************************
 SHOTBL-READ-SEC      SECTION.
     MOVE     "SHOTBL-READ-SEC"   TO   S-NAME.
*
     MOVE      STM-F01       TO   STB-F01.
     MOVE      STM-F02       TO   STB-F02.
*
     READ      SHOTBL
               INVALID
               MOVE     "INV"      TO   INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   INV-FLG
     END-READ.
*
 SHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル登録                   2.3       *
****************************************************************
 SHOTBL-WRITE-SEC          SECTION.
     MOVE     "SHOTBL-WRITE-SEC"       TO   S-NAME.
*
     INITIALIZE         STB-REC.
     MOVE    STM-F01    TO   STB-F01.
     MOVE    STM-F02    TO   STB-F02.
     MOVE    STM-F031   TO   STB-F031.
     MOVE    STM-F032   TO   STB-F032.
     MOVE    STM-F04    TO   STB-F04.
     MOVE    STM-F05    TO   STB-F05.
     MOVE    STM-F06    TO   STB-F06.
     MOVE    STM-F07    TO   STB-F07.
     MOVE    STM-F08    TO   STB-F08.
     MOVE    STM-F09    TO   STB-F09.
     MOVE    STM-F10    TO   STB-F10.
     MOVE    STM-F11    TO   STB-F11.
*****MOVE    STM-F12    TO   STB-F12.
     MOVE    STM-F98    TO   STB-F98.
     MOVE    ZERO       TO   STB-F99.
     WRITE   STB-REC.
     ADD     1          TO  OUT-CNT.
*
 SHOTBL-WRITE-EXIT.
     EXIT.
****************************************************************
*             エラーファイル作成                     2.4       *
****************************************************************
 ERRF-WRITE-SEC          SECTION.
     MOVE     "ERRF-WRITE-SEC"       TO   S-NAME.
*
     INITIALIZE         STE-REC.
     MOVE    STM-REC    TO   STE-REC.
     MOVE    "E"        TO   STE-F21.
     WRITE   STE-REC.
     ADD     1          TO  ERR-CNT.
*
 ERRF-WRITE-EXIT.
     EXIT.
*****************<<  END PROGRAM  >>******************

```
