# SSI0112B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSI0112B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　セキチュー支払照合　　　　　　　　*
*    モジュール名　　　　：　支払データ編集　　　　　　　　　　*
*    作成日／更新日　　　：　02/10/08                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　受信した支払データの編集を行なう。*
*                                                              *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0112B.
 AUTHOR.                T.T.
 DATE-WRITTEN.          02/10/08.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   PITCH-2
         YB        IS   PITCH-15
         YB-21     IS   BAIKAKU-15
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払情報データ >>--*
     SELECT   SSIHARAD  ASSIGN         DA-01-S-SSIHARAD
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SII-ST.
*----<< 支払編集データ >>--*
     SELECT   SEKSSIF   ASSIGN         DA-01-S-SEKSSIF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SSIHARAD
                        BLOCK CONTAINS      31   RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  SII-REC.
     03  SII-F01                 PIC  X(01).
     03  SII-F02                 PIC  X(02).
     03  SII-F03                 PIC  X(01).
     03  SII-F04                 OCCURS  2.
         05  SII-F041            PIC  X(09).
         05  SII-F042            PIC  X(05).
         05  SII-F043            PIC  X(04).
         05  SII-F044            PIC  X(02).
         05  SII-F045            PIC  X(06).
         05  SII-F046            PIC  X(06).
         05  SII-F047            PIC  X(10).
         05  SII-F048            PIC  X(01).
         05  SII-F049            PIC  X(01).
         05  SII-F040            PIC  X(09).
     03  SII-F05                 PIC  X(18).
*----<< 支払編集ファイル >>--*
 FD  SEKSSIF
                        BLOCK CONTAINS      40   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     SEKSSIF   OF        XFDLIB
              JOINING   SEK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI0112B".
 01  FLGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  RD-CNT         PIC  9(07)     VALUE  ZERO.
     03  OUT-CNT        PIC  9(07)     VALUE  ZERO.
     03  IX             PIC  9(01)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SII-ST             PIC  X(02).
 01  SEK-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*金額変換（１０桁）
 01  WK-SIHARAI         PIC  X(09).
 01  WK-SIHARAI-R       REDEFINES      WK-SIHARAI.
     03  HEN-SIHARAI    PIC  9(09).
*金額変換（６桁）
 01  WK-SOUKIN          PIC  X(06).
 01  WK-SOUKIN-R        REDEFINES      WK-SOUKIN.
     03  HEN-SOUKIN     PIC  9(06).
*取引先変換
 01  WK-TORIHIKI        PIC  X(06).
 01  WK-TORIHIKI-R      REDEFINES      WK-TORIHIKI.
     03  HEN-TORIHIKI   PIC  9(06).
*店舗変換
 01  WK-TENPO           PIC  X(05).
 01  WK-TENPO-R         REDEFINES      WK-TENPO.
     03  HEN-TENPO      PIC  9(05).
*
 01  WK-TENCD           PIC  X(05).
*金額変換ワーク（１０桁）
 01  WK-HENKAN.
     03  WK-HENKAN-1    PIC  X(01)    VALUE  SPACE.
     03  WK-HENKAN-2    PIC  X(09)    VALUE  SPACE.
 01  WK-HENKAN-KIN      PIC S9(10)    VALUE  ZERO.
*金額変換ワーク（６桁）
 01  WK-HENSOU.
     03  WK-HENSOU-1    PIC  X(01)    VALUE  SPACE.
     03  WK-HENSOU-2    PIC  X(05)    VALUE  SPACE.
 01  WK-HENSOU-KIN      PIC S9(06)    VALUE  ZERO.
*差引支払額エリア
 01  WK-KEISAN-AREA.
     03  WK-SASIHIKI    PIC S9(10)    VALUE  ZERO.
     03  WK-SIHAKIN     PIC S9(10)    VALUE  ZERO.
     03  WK-SOUSAI      PIC S9(10)    VALUE  ZERO.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計データ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SSIHARAD.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SSIHARAD  ERROR " SII-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払編集データ >>--*
 SEK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEKSSIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### " PG-ID "  SEKSSIF  ERROR " SEK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
*
*終了メッサージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SSIHARAD.
     OPEN     OUTPUT    SEKSSIF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
*----<< 初期読込み >>-*
     PERFORM  900-SII-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     PERFORM  VARYING  IX  FROM  1  BY  1  UNTIL  IX  >  2
              IF  SII-F041(IX)  NOT =  SPACE
                  MOVE  SPACE        TO    SEK-REC
                  INITIALIZE               SEK-REC
                  MOVE  SII-F01      TO    SEK-F01
                  MOVE  SII-F02      TO    SEK-F02
                  MOVE  SII-F03      TO    SEK-F03
                  MOVE  SII-F041(IX) TO    WK-SIHARAI
                  MOVE  HEN-SIHARAI  TO    SEK-F04
                  MOVE  SII-F042(IX) TO    WK-TENPO
                  MOVE  WK-TENPO     TO    SEK-F05
                  MOVE  SII-F043(IX) TO    SEK-F06
                  MOVE  SII-F044(IX) TO    SEK-F07
                  MOVE  SII-F045(IX) TO    WK-SOUKIN
                  MOVE  WK-SOUKIN    TO    SEK-F08
                  MOVE  SII-F046(IX) TO    WK-SOUKIN
                  MOVE  WK-SOUKIN    TO    SEK-F09
                  MOVE  SPACE        TO    WK-HENKAN
                  MOVE  SII-F047(IX) TO    WK-HENKAN
                  PERFORM  900-HENKAN-RTN
                  MOVE  WK-HENKAN-KIN TO   SEK-F10
                  MOVE  SII-F048(IX) TO    SEK-F11
                  MOVE  SII-F049(IX) TO    SEK-F12
                  MOVE  SII-F040(IX) TO    SEK-F13
                  WRITE SEK-REC
                  ADD   1            TO    OUT-CNT
              END-IF
     END-PERFORM.
*----<< 読込み >>-*
     PERFORM  900-SII-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    SSIHARAD.
     CLOSE    SEKSSIF.
*
     DISPLAY "READ-CNT  = "  RD-CNT  UPON  CONS.
     DISPLAY "WRITE-CNT = "  OUT-CNT UPON  CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払情報データ　　 READ                      *
*--------------------------------------------------------------*
 900-SII-READ           SECTION.
     READ     SSIHARAD  AT   END
              MOVE      "END"        TO     END-FLG
              GO   TO   900-SII-READ-EXIT
     END-READ.
*
     IF   SII-F01  =  "B"
     AND  SII-F02  =  "21"
     AND  SII-F03  =  "M"
          ADD          1             TO     RD-CNT
     ELSE
          GO                         TO     900-SII-READ
     END-IF.
*
 900-SII-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    金額編集処理                                 *
*--------------------------------------------------------------*
 900-HENKAN-RTN         SECTION.
*
     IF       WK-HENKAN-1    =    "-"
              MOVE WK-HENKAN-2 TO WK-SIHARAI
              COMPUTE WK-HENKAN-KIN = HEN-SIHARAI * (-1)
     ELSE
              MOVE WK-HENKAN-2 TO WK-SIHARAI
              MOVE HEN-SIHARAI TO WK-HENKAN-KIN
     END-IF.
*
 900-HENKAN-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
