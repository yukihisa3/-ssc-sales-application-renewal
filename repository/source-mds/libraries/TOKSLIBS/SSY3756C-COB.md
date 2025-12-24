# SSY3756C

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3756C.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコト新ＥＤＩシステム　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　              *
*    モジュール名　　　　：　出荷依頼データＣＳＶ出力          *
*    作成日／更新日　　　：　10/10/07                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ナフコ出荷依頼データをＣＳＶに　　*
*    　　　　　　　　　　　　出力する。　　　　　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3756C.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/07.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<ナフコ出荷指示Ｆ >>********************************
     SELECT   NFSYUKF           ASSIGN    TO   DA-01-VI-NFSYUKL4
                                ORGANIZATION   INDEXED
                                ACCESS  MODE   SEQUENTIAL
                                RECORD  KEY    NFS-F04  NFS-F06
                                               NFS-F13
                                STATUS         NFS-STATUS.
*
*****<<店舗テーブル    >>*************************************
     SELECT   NFTNTBL           ASSIGN    TO   FNTNTBL
                                STATUS         NFT-STATUS.
*
*****<<出荷指示ﾃﾞｰﾀCSV >>*************************************
     SELECT   NFSYKDT           ASSIGN    TO   NFSYKDT
                                STATUS         NFD-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ出荷指示データ                               *
*--------------------------------------------------------------*
 FD  NFSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     NFSYUKF   OF        XFDLIB
              JOINING   NFS       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗テーブル　　　　　　　　　　                   *
*--------------------------------------------------------------*
 FD  NFTNTBL           BLOCK CONTAINS 1    RECORDS.
 01  NFT-REC.
     03  NFT-F01        PIC       X(005).
     03  NFT-F02        PIC       9(003).
     03  NFT-FR         PIC       X(048).
*
*--------------------------------------------------------------*
*    FILE = ナフコ出荷指示データＣＳＶ　　　                   *
*--------------------------------------------------------------*
 FD  NFSYKDT           BLOCK CONTAINS 1    RECORDS.
 01  NFD-REC.
     03  FILLER         PIC       X(500).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  END-FLG2                     PIC       X(03)  VALUE  SPACE.
 01  SET-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  NFS-STATUS                   PIC       X(02).
 01  NFD-STATUS                   PIC       X(02).
 01  NFT-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  READ2-CNT                    PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  IX1                          PIC       9(04)  VALUE  ZERO.
***** 集計項目
 01  WK-SYUKSU                    PIC       9(07)  VALUE  ZERO.
 01  WK-CACESU                    PIC       9(05)  VALUE  ZERO.
***** ブレイクキー
 01  BRK-KEY.
     03  BRK-TRICD                PIC       9(05)  VALUE  ZERO.
     03  BRK-TENCD                PIC       9(03)  VALUE  ZERO.
     03  BRK-SHOCD                PIC       X(08)  VALUE  SPACE.
***** 店舗テーブル
 01  TBL-TENPO.
     03  TBL-TENPOR               OCCURS    2000.
         05   TBL-F01             PIC       9(05)  VALUE  ZERO.
         05   TBL-F02             PIC       9(03)  VALUE  ZERO.
*タイトルエリア
*見出しエリア
 01  WK-HEAD.
     03  NFD-H01       PIC X(02).
     03  NFD-HA01      PIC X(01).
     03  NFD-H02       PIC 9(03).
     03  NFD-HA02      PIC X(01).
     03  NFD-H03       PIC 9(05).
     03  NFD-HA03      PIC X(01).
     03  NFD-HN01      PIC X(01).
     03  NFD-H04       PIC N(10).
     03  NFD-HN02      PIC X(01).
     03  NFD-HA04      PIC X(01).
     03  NFD-H05       PIC 9(08).
     03  NFD-HA05      PIC X(01).
     03  NFD-H06       PIC 9(04).
     03  NFD-HA06      PIC X(01).
     03  NFD-H07       PIC 9(08).
     03  NFD-HA07      PIC X(01).
     03  FILLER        PIC X(19).

*明細エリア
 01  WK-MEISAI.
     03  NFD-M01       PIC X(02).
     03  NFD-A01       PIC X(01).
     03  NFD-M02       PIC X(02).
     03  NFD-A02       PIC X(01).
     03  NFD-AN01      PIC X(01).
     03  NFD-M03       PIC N(10).
     03  NFD-AN02      PIC X(01).
     03  NFD-A03       PIC X(01).
     03  NFD-M04       PIC X(16).
     03  NFD-A04       PIC X(01).
     03  NFD-M05       PIC X(08).
     03  NFD-A05       PIC X(01).
     03  NFD-AN03      PIC X(01).
     03  NFD-M06       PIC N(10).
     03  NFD-AN04      PIC X(01).
     03  NFD-A06       PIC X(01).
     03  NFD-AN05      PIC X(01).
     03  NFD-M07       PIC N(10).
     03  NFD-AN06      PIC X(01).
     03  NFD-A07       PIC X(01).
     03  NFD-M08       PIC 9(08).
     03  NFD-A08       PIC X(01).
     03  NFD-M09       PIC 9(06).
     03  NFD-A09       PIC X(01).
     03  NFD-M10       PIC 9(03).
     03  NFD-A10       PIC X(01).
     03  NFD-M11       PIC 9(09).
     03  NFD-A11       PIC X(01).
     03  NFD-M12       PIC 9(02).
     03  NFD-A12       PIC X(01).
     03  NFD-M13       PIC 9(08).
     03  NFD-A13       PIC X(01).
     03  NFD-M14       PIC 9(08).
     03  NFD-A14       PIC X(01).
     03  NFD-M15       PIC 9(08).
     03  NFD-A15       PIC X(01).
     03  NFD-M16       PIC 9(07).
     03  NFD-A16       PIC X(01).
     03  NFD-M17       PIC 9(07).
     03  NFD-A17       PIC X(01).
     03  NFD-M18       PIC 9(05).
     03  NFD-A18       PIC X(01).
     03  NFD-M19       PIC 9(08).
     03  NFD-A19       PIC X(01).
     03  NFD-M20       PIC 9(04).
     03  NFD-A20       PIC X(01).
     03  NFD-M21       PIC 9(08).
     03  NFD-A21       PIC X(01).
     03  FILLER        PIC X(20).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3756C".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3756C".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3756C".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE                SECTION.
 01  LINK-IN-TAB        PIC  X(02).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-IN-TAB.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSYUKF.
     MOVE     "NFSYUKF "          TO        ERR-FL-ID.
     MOVE     NFS-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFTNTBL.
     MOVE     "NFTNTBL"           TO        ERR-FL-ID.
     MOVE     NFT-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSYKDT.
     MOVE     "NFSYKDT"          TO        ERR-FL-ID.
     MOVE     NFD-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3756C-START         SECTION.
*
     MOVE   "SSY3756C-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY3756C-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFSYUKF.
     OPEN     INPUT     NFTNTBL.
     OPEN     EXTEND    NFSYKDT.
     DISPLAY  MSG-START UPON CONS.
*  初期化
     MOVE     SPACE               TO   WK-HEAD.
     MOVE     SPACE               TO   WK-MEISAI.
     INITIALIZE                        WK-HEAD.
     INITIALIZE                        WK-MEISAI.
*
     MOVE  ALL "9"    TO  TBL-TENPO.
*
     PERFORM  NFSYUKF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              PERFORM  NFTNTBL-RD-SEC
              MOVE     1            TO    IX1
              PERFORM  UNTIL  END-FLG2  =  "END"  OR
                              IX1  >  2000
                 MOVE   NFT-F01     TO    TBL-F01 (IX1)
                 MOVE   NFT-F02     TO    TBL-F02 (IX1)
                 ADD    1           TO    IX1
                 PERFORM  NFTNTBL-RD-SEC
              END-PERFORM
     END-IF.
*
     IF     LINK-IN-TAB    =   "DT"
            MOVE  NFS-HE06   TO      BRK-TRICD
            MOVE  NFS-F06    TO      BRK-TENCD
            MOVE  NFS-F13    TO      BRK-SHOCD
************ADD   NFS-F19    TO      WK-SYUKSU
************ADD   NFS-F18    TO      WK-CACESU
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ出荷指示Ｆ読込　　　　　　
****************************************************************
 NFSYUKF-RD-SEC             SECTION.
*
     MOVE    "NFSYUKF-RD-SEC"    TO   S-NAME.
*
     READ     NFSYUKF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 NFSYUKF-RD-EXIT.
     EXIT.
*
****************************************************************
*    店舗テーブル読み込み　　　　　　
****************************************************************
 NFTNTBL-RD-SEC             SECTION.
*
     MOVE    "NFTNTBL-RD-SEC"    TO   S-NAME.
*
     READ     NFTNTBL
          AT END
              MOVE     "END"      TO        END-FLG2
          NOT  AT  END
              ADD       1         TO        READ2-CNT
     END-READ.
*
 NFTNTBL-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*項目転送
     IF     LINK-IN-TAB    =   "HD"
*初期化
            MOVE     SPACE        TO   WK-HEAD
            INITIALIZE                 WK-HEAD
*
            MOVE    X"28"         TO   NFD-HN01
            MOVE    X"29"         TO   NFD-HN02
*
            MOVE     ","          TO   NFD-HA01 NFD-HA02
            MOVE     ","          TO   NFD-HA03 NFD-HA04
            MOVE     ","          TO   NFD-HA05 NFD-HA06
            MOVE     ","          TO   NFD-HA07
*
            MOVE     "HD"         TO   NFD-H01
*
         IF    BRK-TRICD     NOT =  NFS-HE06  OR
               BRK-TENCD     NOT =  NFS-F06
            MOVE     NFS-HE06     TO   BRK-TRICD
            MOVE     NFS-F06      TO   BRK-TENCD
*
            MOVE     SPACE        TO   SET-FLG
            PERFORM  VARYING  IX1  FROM  1  BY  1
                              UNTIL  SET-FLG  =  "1"   OR
                                     IX1      >  2000  OR
                                     TBL-F01(IX1)  =  99999
                 IF    TBL-F01 (IX1)        =   NFS-F06
                       MOVE   TBL-F02(IX1)   TO    NFD-H02
                       MOVE   "1"            TO    SET-FLG
                 END-IF
            END-PERFORM
*
            MOVE     NFS-F06      TO   NFD-H03
            MOVE     NFS-F37      TO   NFD-H04
            MOVE     NFS-F02      TO   NFD-H05
            MOVE     NFS-F03      TO   NFD-H06
            MOVE     NFS-F04      TO   NFD-H07
            MOVE     WK-HEAD      TO   NFD-REC
            WRITE    NFD-REC
            ADD      1            TO   OUTPUT-CNT
         END-IF
     END-IF.
*
     IF     LINK-IN-TAB    =   "DT"
*
         IF    BRK-TRICD     NOT =  NFS-HE06  OR
               BRK-TENCD     NOT =  NFS-F06   OR
               BRK-SHOCD     NOT =  NFS-F13
               MOVE    NFS-HE06      TO   BRK-TRICD
               MOVE    NFS-F06       TO   BRK-TENCD
               MOVE    NFS-F13       TO   BRK-SHOCD
*
               MOVE     WK-SYUKSU    TO   NFD-M16
               MOVE     WK-CACESU    TO   NFD-M17
               MOVE     WK-MEISAI    TO   NFD-REC
               WRITE    NFD-REC
               ADD      1            TO   OUTPUT-CNT
*初期化
               MOVE     SPACE        TO   WK-MEISAI
               INITIALIZE                 WK-HEAD
*
               MOVE     ZERO         TO   WK-SYUKSU
               MOVE     ZERO         TO   WK-CACESU
*****          ADD      NFS-F19      TO   WK-SYUKSU
*****          ADD      NFS-F18      TO   WK-CACESU
         END-IF
*
         MOVE    X"28"         TO   NFD-AN01
         MOVE    X"28"         TO   NFD-AN03
         MOVE    X"28"         TO   NFD-AN05
         MOVE    X"29"         TO   NFD-AN02
         MOVE    X"29"         TO   NFD-AN04
         MOVE    X"29"         TO   NFD-AN06
*
         MOVE     ","       TO   NFD-A01 NFD-A02 NFD-A03
         MOVE     ","       TO   NFD-A04 NFD-A05 NFD-A06
         MOVE     ","       TO   NFD-A07 NFD-A08 NFD-A09
         MOVE     ","       TO   NFD-A10 NFD-A11 NFD-A12
         MOVE     ","       TO   NFD-A13 NFD-A14 NFD-A15
         MOVE     ","       TO   NFD-A16 NFD-A17 NFD-A18
         MOVE     ","       TO   NFD-A19 NFD-A20 NFD-A21
*
         MOVE     "DT"         TO   NFD-M01
         MOVE     NFS-F05      TO   NFD-M02
         MOVE     NFS-F34      TO   NFD-M03
         MOVE     NFS-F15      TO   NFD-M04(1:8)
         MOVE     NFS-F16      TO   NFD-M04(9:8)
         MOVE     NFS-F13      TO   NFD-M05
         MOVE     NFS-F35      TO   NFD-M06
         MOVE     NFS-F36      TO   NFD-M07
         MOVE     NFS-F01      TO   NFD-M08
         MOVE     NFS-F18      TO   NFD-M09
*
         MOVE     SPACE        TO   SET-FLG
         PERFORM  VARYING  IX1  FROM  1  BY  1
                              UNTIL  SET-FLG  =  "1"   OR
                                     IX1      >  2000  OR
                                     TBL-F01(IX1)  =  99999
             IF    TBL-F01 (IX1)   =   NFS-F06
                   MOVE   TBL-F02(IX1)   TO    NFD-M10
                   MOVE   "1"            TO    SET-FLG
             END-IF
         END-PERFORM
*
         MOVE     NFS-F07      TO   NFD-M11
         MOVE     NFS-F08      TO   NFD-M12
         MOVE     NFS-F09      TO   NFD-M13
         MOVE     NFS-F11      TO   NFD-M14
         MOVE     NFS-F10      TO   NFD-M15
         ADD      NFS-F20      TO   WK-SYUKSU
         ADD      NFS-F22      TO   WK-CACESU
         MOVE     NFS-F06      TO   NFD-M18
         MOVE     NFS-F02      TO   NFD-M19
         MOVE     NFS-F03      TO   NFD-M20
         MOVE     NFS-F04      TO   NFD-M21
     END-IF.
*    次レコード読込み
     PERFORM  NFSYUKF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF     LINK-IN-TAB    =   "DT"
            MOVE     WK-SYUKSU    TO   NFD-M16
            MOVE     WK-CACESU    TO   NFD-M17
            MOVE     WK-MEISAI    TO   NFD-REC
            WRITE    NFD-REC
            ADD      1            TO   OUTPUT-CNT
     END-IF.
*
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    NFSYUKF   NFTNTBL  NFSYKDT.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.

```
