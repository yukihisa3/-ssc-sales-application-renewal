****************************************************************
*    顧客名　　　　　　　：　サカタのタネ殿　　　　　　　　　  *
*    業務名　　　　　　　：　ナフコ新ＥＤＩシステム            *
*    モジュール名　　　　：　受領アンマッチＣＳＶ出力　　　　　*
*    作成日／更新日　　　：　12/04/02                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　新受領アンマッチデータを読み、受領*
*                        ：　アンマッチＣＳＶを出力する。      *
*    作成日／更新日　　　：　12/06/01                          *
*                        ：　ＣＳＶレイアウト変更              *
*    作成日／更新日　　　：　16/10/05                          *
*                        ：　ナフコ商品マスタの索引方法変更    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3714V.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          08/11/17.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 新受領アンマッチデータ >>--*
     SELECT   NFNUNMF   ASSIGN         DA-01-VI-NFNUNML1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  UNM-F01  UNM-F02  UNM-F03
                        STATUS         UNM-ST.
*----<< ナフコ商品マスタ >>--*
     SELECT   NFSHOMS   ASSIGN         DA-01-VI-NFSHOMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01
                        STATUS         SHO-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52  TEN-F011
                        STATUS         TEN-ST.
*----<< 作場マスタ >>--*
     SELECT   SAKUBAF   ASSIGN         DA-01-VI-SAKUBAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SYK-F01
                        STATUS         SYK-ST.
*----<< アンマッチＣＳＶ >>-*
     SELECT   NFNUNMWK  ASSIGN         DA-01-S-NFNUNMWK
                        STATUS         UWK-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 新受領アンマッチデータ >>--*
 FD  NFNUNMF            LABEL RECORD   IS   STANDARD.
     COPY     NFNUNMF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*----<< ナフコ商品マスタ >>--*
 FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   SHO       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< 作場マスタ >>--*
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SYK       PREFIX.
*----<< アンマッチＣＳＶ >>-*
 FD  NFNUNMWK
                   BLOCK CONTAINS 1 RECORDS.
 01  UWK-REC.
     03  FILLER        PIC  X(1000).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  HTENMS-INV-FLG PIC  X(03)     VALUE  SPACE.
     03  NFSHOMS-INV-FLG PIC X(03)     VALUE  SPACE.
     03  SAKUBAF-INV-FLG PIC X(03)     VALUE  SPACE.
     03  PAGE-FLG       PIC  X(01)     VALUE  SPACE.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT         PIC  9(07)     VALUE  ZERO.
 01  WK-UNM-F01         PIC  9(05)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  UNM-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  SYK-ST             PIC  X(02).
 01  UWK-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSY3714V".
*----<< WORK     >>--*
 01  UWK-WT-CNT         PIC  9(07)     VALUE   ZERO.
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
 01  WK-KEI-YMD.
     03  WK-KEI-YYYY    PIC  9(04).
     03  WK-KEI-KU1     PIC  X(01).
     03  WK-KEI-MM      PIC  9(02).
     03  WK-KEI-KU2     PIC  X(01).
     03  WK-KEI-DD      PIC  9(02).
*--<< タイトル行 >>-*
 01  HD01.
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"店舗ＣＤ".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"店舗名".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"伝票番号".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"元伝票番号".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(04)  VALUE NC"受領存在".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(04)  VALUE NC"出荷存在".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"理由１".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(07)  VALUE NC"ナフコ起票伝票".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由３".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由４".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由５".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由６".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由７".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由８".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"理由９".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(04)  VALUE NC"理由１０".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"受領計上日".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(06)  VALUE NC"受領相手商品".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"商品名".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(07)  VALUE NC"受領区分名".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(06)  VALUE NC"赤黒区分名".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"受領数".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"原単価".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(03)  VALUE NC"売単価".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(04)  VALUE NC"理由名".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"連絡事項".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"出荷納品日".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"出荷数".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(07)  VALUE NC"出荷原単価".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"出荷売単価".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(05)  VALUE NC"出荷場所名".
     03  FILLER            PIC  X(01)  VALUE X"29".
     03  FILLER            PIC  X(01)  VALUE ",".
     03  FILLER            PIC  X(01)  VALUE X"28".
     03  FILLER            PIC  N(06)  VALUE NC"出荷相手商品".
     03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(04)  VALUE NC"出荷場所".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(05)  VALUE NC"出荷場所名".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(06)  VALUE NC"出荷相手商品".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(03)  VALUE NC"出荷数".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(05)  VALUE NC"出荷原単価".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*I   03  FILLER            PIC  X(01)  VALUE ",".
*I   03  FILLER            PIC  X(01)  VALUE X"28".
*I   03  FILLER            PIC  N(05)  VALUE NC"出荷売単価".
*I   03  FILLER            PIC  X(01)  VALUE X"29".
*
 01  MEISAI.
     03  MS-TENCD          PIC  9(05).
     03  MS-KU1            PIC  X(01).
     03  MS-TENCDS         PIC  X(01).
     03  MS-TENNM          PIC  N(10).
     03  MS-TENCDE         PIC  X(01).
     03  MS-KU2            PIC  X(01).
     03  MS-DENNO          PIC  9(09).
     03  MS-KU3            PIC  X(01).
     03  MS-MOTODEN        PIC  9(09).
     03  MS-KU4            PIC  X(01).
     03  MS-RIYU1S         PIC  X(01).
     03  MS-RIYU1          PIC  N(09).
     03  MS-RIYU1E         PIC  X(01).
*I   03  MS-JYUSONS        PIC  X(01).
*I   03  MS-JYUSON         PIC  N(04).
*I   03  MS-JYUSONE        PIC  X(01).
*I   03  MS-KU5            PIC  X(01).
*I   03  MS-SYUSONS        PIC  X(01).
*I   03  MS-SYUSON         PIC  N(04).
*I   03  MS-SYUSONE        PIC  X(01).
     03  MS-KU5            PIC  X(01).
*I   03  MS-KU52           PIC  X(01).
*I   03  MS-RIYU2S         PIC  X(01).
*I   03  MS-RIYU2          PIC  N(08).
*I   03  MS-RIYU2E         PIC  X(01).
*I   03  MS-KU6            PIC  X(01).
*I   03  MS-RIYU3S         PIC  X(01).
*I   03  MS-RIYU3          PIC  N(08).
*I   03  MS-RIYU3E         PIC  X(01).
*I   03  MS-KU7            PIC  X(01).
*I   03  MS-RIYU4S         PIC  X(01).
*I   03  MS-RIYU4          PIC  N(08).
*I   03  MS-RIYU4E         PIC  X(01).
*I   03  MS-KU8            PIC  X(01).
     03  MS-RIYU5S         PIC  X(01).
     03  MS-RIYU5          PIC  N(08).
     03  MS-RIYU5E         PIC  X(01).
     03  MS-KU6            PIC  X(01).
*I   03  MS-RIYU6S         PIC  X(01).
*I   03  MS-RIYU6          PIC  N(08).
*I   03  MS-RIYU6E         PIC  X(01).
*I   03  MS-KU10           PIC  X(01).
*I   03  MS-RIYU7S         PIC  X(01).
*I   03  MS-RIYU7          PIC  N(08).
*I   03  MS-RIYU7E         PIC  X(01).
*I   03  MS-KU11           PIC  X(01).
*I   03  MS-RIYU8S         PIC  X(01).
*I   03  MS-RIYU8          PIC  N(08).
*I   03  MS-RIYU8E         PIC  X(01).
*I   03  MS-KU12           PIC  X(01).
*I   03  MS-RIYU9S         PIC  X(01).
*I   03  MS-RIYU9          PIC  N(08).
*I   03  MS-RIYU9E         PIC  X(01).
*I   03  MS-KU13           PIC  X(01).
*I   03  MS-RIYUAS         PIC  X(01).
*I   03  MS-RIYUA          PIC  N(08).
*I   03  MS-RIYUAE         PIC  X(01).
*I   03  MS-KU14           PIC  X(01).
     03  MS-JYUKEIJYO      PIC  9(08).
     03  MS-KU7            PIC  X(01).
*I   03  MS-JYUDENKU       PIC  9(02).
*I   03  MS-KU16           PIC  X(01).
*I   03  MS-JYUDENKUNMS    PIC  X(01).
*I   03  MS-JYUDENKUNM     PIC  N(04).
*I   03  MS-JYUDENKUNME    PIC  X(01).
*I   03  MS-KU17           PIC  X(01).
     03  MS-JYUSYOCD       PIC  X(13).
     03  MS-KU8            PIC  X(01).
*****03  MS-JYUSYONMS      PIC  X(01).
     03  MS-JYUSYONM       PIC  X(40).
*****03  MS-JYUSYONME      PIC  X(01).
     03  MS-KU9            PIC  X(01).
     03  MS-JYUKBNNMS      PIC  X(01).
     03  MS-JYUKBNNM       PIC  N(04).
     03  MS-JYUKBNNME      PIC  X(01).
     03  MS-KU10           PIC  X(01).
     03  MS-AKAKURONMS     PIC  X(01).
     03  MS-AKAKURONM      PIC  N(04).
     03  MS-AKAKURONME     PIC  X(01).
     03  MS-KU11           PIC  X(01).
     03  MS-JYUSU          PIC  9(07).
     03  MS-KU12           PIC  X(01).
     03  MS-JYUGEN         PIC  9(07).
     03  MS-KU13           PIC  X(01).
     03  MS-JYUBAI         PIC  9(07).
     03  MS-KU14           PIC  X(01).
*I   03  MS-JYUKBN         PIC  9(02).
*I   03  MS-KU22           PIC  X(01).
*I   03  MS-AKAKURO        PIC  9(02).
*I   03  MS-KU24           PIC  X(01).
*I   03  MS-KU25           PIC  X(01).
*I   03  MS-RIYUKBN        PIC  9(02).
*I   03  MS-RIYUCD         PIC  9(02).
*I   03  MS-KU26           PIC  X(01).
     03  MS-RIYUNMS        PIC  X(01).
     03  MS-RIYUNM         PIC  N(10).
     03  MS-RIYUNME        PIC  X(01).
     03  MS-KU15           PIC  X(01).
     03  MS-RENRAKUS       PIC  X(01).
     03  MS-RENRAKU        PIC  N(20).
     03  MS-RENRAKUE       PIC  X(01).
     03  MS-KU16           PIC  X(01).
     03  MS-SYUDATE        PIC  9(08).
     03  MS-KU17           PIC  X(01).
*I   03  MS-SYUKABA        PIC  X(02).
*I   03  MS-KU30           PIC  X(01).
     03  MS-SYUSU          PIC  9(07).
     03  MS-KU18           PIC  X(01).
     03  MS-SYUGEN         PIC  9(07).
     03  MS-KU19           PIC  X(01).
     03  MS-SYUBAI         PIC  9(07).
     03  MS-KU20           PIC  X(01).
     03  MS-SYUKABANMS     PIC  X(01).
     03  MS-SYUKABANM      PIC  N(10).
     03  MS-SYUKABANME     PIC  X(01).
     03  MS-KU21           PIC  X(01).
     03  MS-SYUSYOCD       PIC  X(13).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 新受領アンマッチデータ >>--*
 NFNUNMF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFNUNMF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " NFNUNML1  ERROR " UNM-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ナフコ商品マスタ >>--*
 NFSHOMS-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFSHOMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID "  NFSHOMS1 ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 作場マスタ >>--*
 SAKUBAF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SAKUBAF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " SAKUBAF  ERROR " SYK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< アンマッチＣＳＶ >>--*
 NFNUNMWK-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NFNUNMWK.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID " NFNUNMWK ERROR " UWK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*プログラムコントロール
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    "END".
     PERFORM  300-END-RTN.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     NFNUNMF.
     OPEN     INPUT     NFSHOMS.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     SAKUBAF.
     OPEN     OUTPUT    NFNUNMWK.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*新受領アンマッチデータ読込
     PERFORM  900-UNM-READ.
     IF   END-FLG  NOT =  "END"
          MOVE  SPACE            TO   UWK-REC
**********INITIALIZE                  UWK-REC
          MOVE  HD01             TO   UWK-REC
          WRITE UWK-REC
          ADD   1                TO   UWK-WT-CNT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*レコード初期化
     MOVE  SPACE                  TO      UWK-REC  MEISAI.
     INITIALIZE                           UWK-REC  MEISAI.
*カンマセット
     MOVE  ","  TO  MS-KU1  MS-KU2  MS-KU3  MS-KU4  MS-KU5
                    MS-KU6  MS-KU7  MS-KU8  MS-KU9  MS-KU10
                    MS-KU11 MS-KU12 MS-KU13 MS-KU14 MS-KU15
                    MS-KU16 MS-KU17 MS-KU18 MS-KU19 MS-KU20
                    MS-KU21.
*I                  MS-KU22 MS-KU23 MS-KU24 MS-KU25
*I                  MS-KU26 MS-KU27 MS-KU28 MS-KU29 MS-KU30
*I                  MS-KU31 MS-KU32 MS-KU33 MS-KU34 MS-KU181
*I                  MS-KU51 MS-KU52.
*制御ＣＤセット
     MOVE X"28" TO  MS-TENCDS
                    MS-RIYU1S
*I                  MS-RIYU2S
*I                  MS-RIYU3S
*I                  MS-RIYU4S
                    MS-RIYU5S
*I                  MS-RIYU6S
*I                  MS-RIYU7S
*I                  MS-RIYU8S
*I                  MS-RIYU9S
*I                  MS-RIYUAS
*I                  MS-JYUDENKUNMS
                    MS-JYUKBNNMS
                    MS-AKAKURONMS
                    MS-RIYUNMS
                    MS-RENRAKUS
                    MS-SYUKABANMS.
*I                  MS-JYUSONS
*I                  MS-SYUSONS.
*
     MOVE X"29" TO  MS-TENCDE
                    MS-RIYU1E
*I                  MS-RIYU2E
*I                  MS-RIYU3E
*I                  MS-RIYU4E
                    MS-RIYU5E
*I                  MS-RIYU6E
*I                  MS-RIYU7E
*I                  MS-RIYU8E
*I                  MS-RIYU9E
*I                  MS-RIYUAE
*I                  MS-JYUDENKUNME
                    MS-JYUKBNNME
                    MS-AKAKURONME
                    MS-RIYUNME
                    MS-RENRAKUE
                    MS-SYUKABANME.
*I                  MS-JYUSONE
*I                  MS-SYUSONE.
*
 MAIN010.
*項目セット
*　店舗ＣＤ／名称
     MOVE    UNM-F01              TO      MS-TENCD.
     MOVE    137607               TO      TEN-F52.
     MOVE    UNM-F01              TO      TEN-F011.
     PERFORM 900-TEN-READ.
     IF  HTENMS-INV-FLG = SPACE
         MOVE  TEN-F03        TO      MS-TENNM
     ELSE
         MOVE  ALL NC"＊"     TO      MS-TENNM
     END-IF.
*　伝票番号
     MOVE     UNM-F02         TO      MS-DENNO.
*　元伝票番号
     MOVE     UNM-F16         TO      MS-MOTODEN.
*　行
*I   MOVE     UNM-F03         TO      MS-GYO.
*　理由１
     IF  UNM-F28  =  "1"
         MOVE NC"出荷データ無"    TO  MS-RIYU1
     END-IF.
*
     IF  UNM-F29  =  "1"
         MOVE NC"受領データ無"    TO  MS-RIYU1
     END-IF.
*
     IF  ( UNM-F30 = "1" ) AND ( UNM-F31 = "1" )
         MOVE NC"数量違い　原価違い"  TO  MS-RIYU1
     END-IF.
*
     IF  ( UNM-F30 = "1" ) AND ( UNM-F31 = " " )
         MOVE NC"数量違い　　　　　"  TO  MS-RIYU1
     END-IF.
*
     IF  ( UNM-F30 = " " ) AND ( UNM-F31 = "1" )
         MOVE NC"　　　　　原価違い"  TO  MS-RIYU1
     END-IF.
*
*　理由２
*I   IF  UNM-F29  =  "1"
*I       MOVE NC"受領データ無"    TO  MS-RIYU2
*I   END-IF.
*　理由３
*I   IF  UNM-F30  =  "1"
*I       MOVE NC"数量違い　　"    TO  MS-RIYU3
*    END-IF.
*　理由４
*I   IF  UNM-F31  =  "1"
*I       MOVE NC"原価違い　　"    TO  MS-RIYU4
*I   END-IF.
*　ナフコ起票伝票
     IF  UNM-F32  =  "1"
         MOVE NC"ナフコ記票伝票"  TO  MS-RIYU5
     END-IF.
*　理由６
*I   IF  UNM-F33  =  "1"
*I       MOVE NC"計上日違い　"    TO  MS-RIYU6
*I   END-IF.
*　理由７
*I   IF  UNM-F34  =  "1"
*I       MOVE SPACE               TO  MS-RIYU7
*I   END-IF.
*　理由８
*I   IF  UNM-F35  =  "1"
*I       MOVE SPACE               TO  MS-RIYU8
*I   END-IF.
*　理由９
*I   IF  UNM-F36  =  "1"
*I       MOVE SPACE               TO  MS-RIYU9
*I   END-IF.
*　理由１０
*I   IF  UNM-F37  =  "1"
*I       MOVE SPACE               TO  MS-RIYUA
*I   END-IF.
*　受領存在
*I   IF  UNM-F26  =  "1"
*I       MOVE NC"受領存在"    TO      MS-JYUSON
*I   END-IF.
*　出荷存在
*I   IF  UNM-F27  =  "1"
*I       MOVE NC"出荷存在"    TO      MS-SYUSON
*I   END-IF.
*　受領存在
*I   IF  UNM-F26  =  SPACE
*I       GO                   TO      MAIN010
*I   END-IF.
*　受領計上日
      MOVE       UNM-F04          TO      MS-JYUKEIJYO.
*　受領商品ＣＤ、商品名
      MOVE       UNM-F05          TO      MS-JYUSYOCD.
      MOVE       UNM-F05          TO      SHO-F01.
      PERFORM  900-SHO-READ.
      IF  NFSHOMS-INV-FLG = SPACE
          MOVE   SHO-F07          TO      MS-JYUSYONM(1:20)
          MOVE   SHO-F08          TO      MS-JYUSYONM(21:20)
      ELSE
**********受領商品ＣＤがＩＮＶの場合、出荷商品ＣＤで検索する
          MOVE       UNM-F21      TO      SHO-F01
          PERFORM  900-SHO-READ
          IF  NFSHOMS-INV-FLG = SPACE
              MOVE   SHO-F07      TO      MS-JYUSYONM(1:20)
              MOVE   SHO-F08      TO      MS-JYUSYONM(21:20)
          ELSE
              MOVE   ALL "*"      TO      MS-JYUSYONM
          END-IF
      END-IF.
*　受領伝票区分／名称
*I    MOVE       UNM-F06          TO      MS-JYUDENKU.
*I    EVALUATE   UNM-F06
*I        WHEN    1   MOVE NC"仕入通"  TO MS-JYUDENKUNM
*I        WHEN    2   MOVE NC"仕入Ｆ"  TO MS-JYUDENKUNM
*I        WHEN   11   MOVE NC"返品　"  TO MS-JYUDENKUNM
*I        WHEN   12   MOVE NC"値引　"  TO MS-JYUDENKUNM
*I        WHEN   61   MOVE NC"相殺　"  TO MS-JYUDENKUNM
*I        WHEN  OTHER MOVE ALL NC"＊"  TO MS-JYUDENKUNM
*I    END-EVALUATE.
*　受領区分名
*I    MOVE       UNM-F10          TO      MS-JYUKBN.
      EVALUATE   UNM-F10
         WHEN    0  MOVE NC"返品値引"  TO MS-JYUKBNNM
         WHEN    1  MOVE NC"予定通り"  TO MS-JYUKBNNM
         WHEN    2  MOVE NC"訂正受領"  TO MS-JYUKBNNM
         WHEN    3  MOVE NC"仕訂赤黒"  TO MS-JYUKBNNM
         WHEN OTHER MOVE ALL NC"＊"    TO MS-JYUKBNNM
      END-EVALUATE.
*　赤黒区分名
*I    MOVE       UNM-F11          TO      MS-AKAKURO.
      EVALUATE   UNM-F11
         WHEN    0  MOVE NC"黒"        TO MS-AKAKURONM
         WHEN    1  MOVE NC"赤"        TO MS-AKAKURONM
         WHEN OTHER MOVE ALL NC"＊"    TO MS-AKAKURONM
      END-EVALUATE.
*　受領数量
      MOVE       UNM-F07          TO      MS-JYUSU.
*　受領原価
      MOVE       UNM-F08          TO      MS-JYUGEN.
*　受領売価
      MOVE       UNM-F09          TO      MS-JYUBAI.
*　理由
*I    MOVE       UNM-F12          TO      MS-RIYUKBN.
*　理由ＣＤ
*I    MOVE       UNM-F13          TO      MS-RIYUCD.
*　理由名
      IF  UNM-F14 NOT = SPACE
          MOVE       UNM-F14      TO      MS-RIYUNM
      ELSE
          MOVE NC"　　　　　　　　　　" TO MS-RIYUNM
      END-IF.
*　連絡事項
      IF  UNM-F15 NOT = SPACE
          MOVE       UNM-F15      TO      MS-RENRAKU
      ELSE
          MOVE       SPACE        TO      MS-RENRAKU
      END-IF.
*　出荷計上日
      MOVE       UNM-F20          TO      MS-SYUDATE.
*　出荷数量
      MOVE       UNM-F23          TO      MS-SYUSU.
*　出荷原価
      MOVE       UNM-F24          TO      MS-SYUGEN.
*　出荷売価
      MOVE       UNM-F25          TO      MS-SYUBAI.
*　出荷場所
*I    MOVE       UNM-F22          TO      MS-SYUKABA.
*　出荷場所名
      MOVE       UNM-F22          TO      SYK-F01.
      PERFORM  900-SYK-READ.
      IF  SAKUBAF-INV-FLG = SPACE
          MOVE   SYK-F02          TO      MS-SYUKABANM
      ELSE
          MOVE   ALL NC"＊"       TO      MS-SYUKABANM
      END-IF.
*　相手商品
      MOVE       UNM-F21          TO      MS-SYUSYOCD.
*　レコードセット
      MOVE       MEISAI           TO      UWK-REC.
      WRITE  UWK-REC.
      ADD        1                TO      UWK-WT-CNT.
*新受領アンマッチデータ読込
     PERFORM  900-UNM-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     CLOSE    NFNUNMF.
     CLOSE    NFSHOMS.
     CLOSE    HTENMS.
     CLOSE    SAKUBAF.
     CLOSE    NFNUNMWK.
*
     DISPLAY "## OUTPUT-CNT = " UWK-WT-CNT " ##" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    新受領アンマッチデータ読込　　　　　　　　　 *
*--------------------------------------------------------------*
 900-UNM-READ           SECTION.
*
     READ     NFNUNMF
         AT END
              MOVE      "END"          TO   END-FLG
              GO                       TO   900-UNM-READ-EXIT
     END-READ.
*
     MOVE     1                        TO   IN-CNT
*
     IF  IN-CNT(5:3) = "000" OR "500"
         DISPLAY "READ-CNT = " IN-CNT  UPON CONS
     END-IF.
*
 900-UNM-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
*
     READ     HTENMS    INVALID
              MOVE     "INV"      TO   HTENMS-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   HTENMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ナフコ商品マスタ　　READ                     *
*--------------------------------------------------------------*
 900-SHO-READ           SECTION.
*
     READ     NFSHOMS   INVALID
              MOVE     "INV"      TO   NFSHOMS-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   NFSHOMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    作場マスタ　　READ                           *
*--------------------------------------------------------------*
 900-SYK-READ           SECTION.
*
     READ     SAKUBAF   INVALID
              MOVE     "INV"      TO   SAKUBAF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   SAKUBAF-INV-FLG
     END-READ.
*
 900-SYK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
