# SSY4308L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4308L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　アークランドサカモトオンライイン  *
*    モジュール名　　　　：　欠品案内エラーリスト              *
*    作成日／更新日　　　：　2002/01/11                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　欠品案内エラーＦを読み、欠品案内エ*
*                        ：　ラーリストを出力する。            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY4308L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/01/11.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     YA            IS        CHR-2
     YB            IS        CHR-15
     YB-21         IS        CHR-21
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<欠品案内エラーファイル>>----*
     SELECT   ARKERRF   ASSIGN    TO        DA-01-S-ARKERRF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   ARE-ST.
*
*----<<プリント>>----*
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        FILE      STATUS    IS   PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<欠品案内エラーファイル>>----*
 FD  ARKERRF            BLOCK     CONTAINS   40  RECORDS
                        LABEL     RECORD     IS  STANDARD.
                        COPY      ARKERRF    OF  XFDLIB
                        JOINING   ARE            PREFIX.
*----<<プリントファイル>>----*
 FD  PRTFILE
     LABEL    RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER         PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)   VALUE SPACE.
     03  ZERO-FLG       PIC  X(01)   VALUE SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  ARE-ST         PIC  X(02).
     03  PRT-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SSY4308L".
 01  PAGE-CNT          PIC  9(05)      VALUE  ZERO.
 01  LINE-CNT          PIC  9(02)      VALUE  ZERO.
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  HD1-00                  PIC  X(08).
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(16)  VALUE
         NC"※※　欠品案内エラーリスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(17)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2                         CHARACTER   TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(26)  VALUE
       NC"※※下記、データは既にアークランドへ送信済です。※※".
*
 01  HD3                       CHARACTER   TYPE  IS  CHR-15.
     03  FILLER                PIC  X(35)  VALUE  SPACE.
     03  FILLER                PIC  N(05) VALUE NC"バッチ番号".
     03  FILLER                PIC  X(20)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE NC"伝票番号".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(01)  VALUE NC"行".
     03  FILLER                PIC  X(10)  VALUE  SPACE.
     03  FILLER                PIC  N(03)  VALUE NC"納品数".
     03  FILLER                PIC  X(07)  VALUE  SPACE.
     03  FILLER                PIC  N(05) VALUE NC"送信済日付".
*
 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE ALL "=".
*
 01  SEN2.
     03  FILLER                  PIC  X(136) VALUE ALL "-".
*
 01  DT1.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(08).
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  DT1-02                  PIC  9(04).
     03  FILLER                  PIC  X(01)  VALUE  "-".
     03  DT1-03                  PIC  9(08).
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-04                  PIC  9(09).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-05                  PIC  Z9.
     03  FILLER                  PIC  X(04)  VALUE  SPACE.
     03  DT1-06                  PIC  ---,---,--9.99.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-07                  PIC  9(08).
*
 01  DT2                         CHARACTER   TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(24)  VALUE
       NC"※※エラーデータはありません。送信ＯＫです。※※".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 ARK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ARKERRF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY4308L ARKERRF ERROR " ARE-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN  UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY4308L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     ARKERRF.
     OPEN     OUTPUT    PRTFILE.
*ヘッダ行印字
*欠品案内エラーファイル読み込み
     PERFORM       ARKERRF-READ-SEC.
*ファイル終了判定
     IF            END-FLG  =  "END"
                   MOVE    "1"      TO          ZERO-FLG
                   PERFORM HEAD-WT-SEC
                   WRITE   PRT-REC  FROM  DT2   AFTER  5
     ELSE
                   MOVE    SPACE    TO          ZERO-FLG
                   PERFORM HEAD-WT-SEC
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*改ページ判定
     IF   LINE-CNT  >   60
          PERFORM   HEAD-WT-SEC
     END-IF.
*項目セット
     MOVE   ARE-F01        TO    DT1-01.
     MOVE   ARE-F02        TO    DT1-02.
     MOVE   ARE-F03        TO    DT1-03.
     MOVE   ARE-F04        TO    DT1-04.
     MOVE   ARE-F05        TO    DT1-05.
     MOVE   ARE-F06        TO    DT1-06.
     MOVE   ARE-F08        TO    DT1-07.
*明細行印字
     WRITE  PRT-REC  FROM  DT1  AFTER  1.
     WRITE  PRT-REC  FROM  SEN2 AFTER  1.
*行カウントアップ
     ADD    1        TO    LINE-CNT.
*欠品案内エラーファイル読み込み
     PERFORM ARKERRF-READ-SEC.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    ARKERRF  PRTFILE.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY4308L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  欠品案内エラーファイル読み込み              *
*--------------------------------------------------------------*
 ARKERRF-READ-SEC            SECTION.
*
     READ     ARKERRF
         AT  END
              MOVE     "END"      TO   END-FLG
     END-READ.
*
 ARKERRF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ヘッダ部出力処理                                 *
*--------------------------------------------------------------*
 HEAD-WT-SEC                  SECTION.
*
     IF    PAGE-CNT  >  ZERO
           WRITE  PRT-REC  AFTER  PAGE
     END-IF.
*頁カウントアップ
     ADD      1                   TO        PAGE-CNT.
*項目設定
***  プログラムＩＤ
     MOVE     PG-ID               TO        HD1-00.
***  日付
     MOVE     "3"                 TO        LINK-IN-KBN.
     MOVE     SYS-DATE            TO        LINK-IN-YMD6.
     MOVE     ZERO                TO        LINK-IN-YMD8.
     MOVE     ZERO                TO        LINK-OUT-RET.
     MOVE     ZERO                TO        LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING        LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD(1:4)   TO        HD1-01.
     MOVE     LINK-OUT-YMD(5:2)   TO        HD1-02.
     MOVE     LINK-OUT-YMD(7:2)   TO        HD1-03.
***  ページ
     MOVE     PAGE-CNT            TO        HD1-04.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  2.
     IF   ZERO-FLG  =  SPACE
          WRITE    PRT-REC      FROM   HD2       AFTER  2
          WRITE    PRT-REC      FROM   SEN1      AFTER  2
     ELSE
          WRITE    PRT-REC      FROM   SEN1      AFTER  4
     END-IF.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN1      AFTER  1.
*印刷制御
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC             AFTER     1.
*
     ADD      9                   TO        LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.

```
